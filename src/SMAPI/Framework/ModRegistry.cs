using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using StardewModdingAPI.Events;
using StardewModdingAPI.Framework.Events;
using StardewModdingAPI.Framework.Exceptions;
using StardewModdingAPI.Framework.Models;
using StardewModdingAPI.Framework.ModHelpers;
using StardewModdingAPI.Framework.ModLoading;
using StardewModdingAPI.Framework.Reflection;
using StardewModdingAPI.Toolkit;
using StardewModdingAPI.Toolkit.Framework.ModData;
using StardewModdingAPI.Toolkit.Serialisation;
using StardewModdingAPI.Toolkit.Utilities;

namespace StardewModdingAPI.Framework
{
    /// <summary>Tracks the installed mods.</summary>
    internal class ModRegistry
    {
        /*********
        ** Fields
        *********/
        public readonly EventManager EventManager;

        private readonly SCore Core;
        private readonly Monitor Monitor;
        private readonly JsonHelper JsonHelper;
        private readonly Reflector Reflection;
        private readonly SConfig Settings;

        private readonly ModResolver ModResolver;
        private readonly ModToolkit ModToolkit;
        private readonly ModDatabase ModDatabase;

        /// <summary>The game instance</summary>
        private SGame GameInstance => this.Core.GameInstance;

        /// <summary>The underlying content manager.</summary>
        private ContentCoordinator ContentCore => this.GameInstance.ContentCore;

        /// <summary>The registered mod data.</summary>
        private readonly List<IModMetadata> Mods = new List<IModMetadata>();

        /// <summary>An assembly full name => mod lookup.</summary>
        private readonly IDictionary<string, IModMetadata> ModNamesByAssembly = new Dictionary<string, IModMetadata>();

        /// <summary>Whether all mod assemblies have been loaded.</summary>
        public bool AreAllModsLoaded { get; set; }

        /// <summary>Whether all mods have been initialised and their <see cref="IMod.Entry"/> method called.</summary>
        public bool AreAllModsInitialised { get; set; }


        /*********
        ** Public methods
        *********/
        public ModRegistry(SCore core, Monitor monitor, JsonHelper jsonHelper, Reflector reflection, SConfig settings)
        {
            this.Core = core;
            this.Monitor = monitor;
            this.JsonHelper = jsonHelper;
            this.Reflection = reflection;
            this.Settings = settings;

            this.EventManager = new EventManager(monitor, this);

            this.ModResolver = new ModResolver(this);
            this.ModToolkit = new ModToolkit();
            this.ModDatabase = this.ModToolkit.GetModDatabase(Constants.ApiMetadataPath);
        }

        public void LoadModsFromPath(string modsPath, Action<IModMetadata[]> onLoaded = null)
        {
            // init paths
            if (!this.VerifyPath(modsPath)) {
                this.Monitor.Log($"Directory {modsPath} does not exist, cannot load mods from this folder.", LogLevel.Warn);
                return;
            }

            this.AreAllModsLoaded = false;

            // load mods
            this.Monitor.Log("Loading mod metadata...", LogLevel.Trace);

            // load manifests
            IModMetadata[] mods = this.ModResolver.ReadManifests(this.ModToolkit, modsPath, this.ModDatabase).ToArray();

            // filter out ignored mods
            foreach (IModMetadata mod in mods.Where(p => p.IsIgnored))
                this.Monitor.Log($"  Skipped {mod.RelativeDirectoryPath} (folder name starts with a dot).", LogLevel.Trace);
            mods = mods.Where(p => !p.IsIgnored).ToArray();

            // load mods
            this.ModResolver.ValidateManifests(mods, Constants.ApiVersion, this.ModToolkit.GetUpdateUrl);
            mods = this.ModResolver.ProcessDependencies(mods, this.ModDatabase).ToArray();

            if (mods.Length > 0)
            {
                this.LoadMods(modsPath, mods);
                this.Core.UpdateWindowTitles();

                if (onLoaded != null)
                    onLoaded.Invoke(mods);
            }
        }

        /// <summary>Register a mod.</summary>
        /// <param name="metadata">The mod metadata.</param>
        public void Add(IModMetadata metadata)
        {
            this.Mods.Add(metadata);
        }

        /// <summary>Track a mod's assembly for use via <see cref="GetFrom"/>.</summary>
        /// <param name="metadata">The mod metadata.</param>
        /// <param name="modAssembly">The mod assembly.</param>
        public void TrackAssemblies(IModMetadata metadata, Assembly modAssembly)
        {
            this.ModNamesByAssembly[modAssembly.FullName] = metadata;
        }

        /// <summary>Get metadata for all loaded mods.</summary>
        /// <param name="assemblyMods">Whether to include SMAPI mods.</param>
        /// <param name="contentPacks">Whether to include content pack mods.</param>
        public IEnumerable<IModMetadata> GetAll(bool assemblyMods = true, bool contentPacks = true)
        {
            IEnumerable<IModMetadata> query = this.Mods;
            if (!assemblyMods)
                query = query.Where(p => p.IsContentPack);
            if (!contentPacks)
                query = query.Where(p => !p.IsContentPack);

            return query;
        }

        /// <summary>Get metadata for a loaded mod.</summary>
        /// <param name="uniqueID">The mod's unique ID.</param>
        /// <returns>Returns the matching mod's metadata, or <c>null</c> if not found.</returns>
        public IModMetadata Get(string uniqueID)
        {
            // normalise search ID
            if (string.IsNullOrWhiteSpace(uniqueID))
                return null;
            uniqueID = uniqueID.Trim();

            // find match
            return this.GetAll().FirstOrDefault(p => p.HasID(uniqueID));
        }

        /// <summary>Get the mod metadata from one of its assemblies.</summary>
        /// <param name="type">The type to check.</param>
        /// <returns>Returns the mod name, or <c>null</c> if the type isn't part of a known mod.</returns>
        public IModMetadata GetFrom(Type type)
        {
            // null
            if (type == null)
                return null;

            // known type
            string assemblyName = type.Assembly.FullName;
            if (this.ModNamesByAssembly.ContainsKey(assemblyName))
                return this.ModNamesByAssembly[assemblyName];

            // not found
            return null;
        }

        /// <summary>Get the friendly name for the closest assembly registered as a source of deprecation warnings.</summary>
        /// <returns>Returns the source name, or <c>null</c> if no registered assemblies were found.</returns>
        public IModMetadata GetFromStack()
        {
            // get stack frames
            StackTrace stack = new StackTrace();
            StackFrame[] frames = stack.GetFrames();
            if (frames == null)
                return null;

            // search stack for a source assembly
            foreach (StackFrame frame in frames)
            {
                MethodBase method = frame.GetMethod();
                IModMetadata mod = this.GetFrom(method.ReflectedType);
                if (mod != null)
                    return mod;
            }

            // no known assembly found
            return null;
        }

        /// <summary>Reload translations for all mods.</summary>
        /// <param name="mods">The mods for which to reload translations.</param>
        public void ReloadTranslations(IEnumerable<IModMetadata> mods)
        {
            foreach (IModMetadata metadata in mods)
            {
                if (metadata.IsContentPack)
                    throw new InvalidOperationException("Can't reload translations for a content pack.");

                // read translation files
                IDictionary<string, IDictionary<string, string>> translations = new Dictionary<string, IDictionary<string, string>>();
                DirectoryInfo translationsDir = new DirectoryInfo(Path.Combine(metadata.DirectoryPath, "i18n"));
                if (translationsDir.Exists)
                {
                    foreach (FileInfo file in translationsDir.EnumerateFiles("*.json"))
                    {
                        string locale = Path.GetFileNameWithoutExtension(file.Name.ToLower().Trim());
                        try
                        {
                            if (this.JsonHelper.ReadJsonFileIfExists(file.FullName, out IDictionary<string, string> data))
                                translations[locale] = data;
                            else
                                metadata.LogAsMod($"Mod's i18n/{locale}.json file couldn't be parsed.", LogLevel.Warn);
                        }
                        catch (Exception ex)
                        {
                            metadata.LogAsMod($"Mod's i18n/{locale}.json file couldn't be parsed: {ex.GetLogSummary()}", LogLevel.Warn);
                        }
                    }
                }

                // validate translations
                foreach (string locale in translations.Keys.ToArray())
                {
                    // skip empty files
                    if (translations[locale] == null || !translations[locale].Keys.Any())
                    {
                        metadata.LogAsMod($"Mod's i18n/{locale}.json is empty and will be ignored.", LogLevel.Warn);
                        translations.Remove(locale);
                        continue;
                    }

                    // handle duplicates
                    HashSet<string> keys = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase);
                    HashSet<string> duplicateKeys = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase);
                    foreach (string key in translations[locale].Keys.ToArray())
                    {
                        if (!keys.Add(key))
                        {
                            duplicateKeys.Add(key);
                            translations[locale].Remove(key);
                        }
                    }
                    if (duplicateKeys.Any())
                        metadata.LogAsMod($"Mod's i18n/{locale}.json has duplicate translation keys: [{string.Join(", ", duplicateKeys)}]. Keys are case-insensitive.", LogLevel.Warn);
                }

                // update translation
                TranslationHelper translationHelper = (TranslationHelper)metadata.Mod.Helper.Translation;
                translationHelper.SetTranslations(translations);
            }
        }

        /*********
        ** Private methods
        *********/

        /// <summary>Verifies if the path exists.</summary>
        /// <param name="path">The directory path.</param>
        private bool VerifyPath(string path)
        {
            return Directory.Exists(path);
        }

        /// <summary>Load and hook up the given mods.</summary>
        /// <param name="modsPath">The source path from where this mod is being loaded.</param>
        /// <param name="mods">The mods to load.</param>
        private void LoadMods(string modsPath, IModMetadata[] mods)
        {
            this.Monitor.Log($"Attempting to load {mods.Length} mods...", LogLevel.Trace);

            // load mods
            IDictionary<IModMetadata, Tuple<string, string>> skippedMods = new Dictionary<IModMetadata, Tuple<string, string>>();
            using (AssemblyLoader modAssemblyLoader = new AssemblyLoader(Constants.Platform, this.Monitor, this.Settings.ParanoidWarnings))
            {
                // init
                HashSet<string> suppressUpdateChecks = new HashSet<string>(this.Settings.SuppressUpdateChecks, StringComparer.InvariantCultureIgnoreCase);
                InterfaceProxyFactory proxyFactory = new InterfaceProxyFactory();
                void LogSkip(IModMetadata mod, string errorPhrase, string errorDetails)
                {
                    skippedMods[mod] = Tuple.Create(errorPhrase, errorDetails);
                    if (mod.Status != ModMetadataStatus.Failed)
                        mod.SetStatus(ModMetadataStatus.Failed, errorPhrase);
                }

                // load mods
                foreach (IModMetadata contentPack in mods)
                {
                    if (!this.TryLoadMod(modsPath, contentPack, mods, modAssemblyLoader, proxyFactory, suppressUpdateChecks, out string errorPhrase, out string errorDetails))
                        LogSkip(contentPack, errorPhrase, errorDetails);
                }
            }

            IModMetadata[] loadedContentPacks = mods.Where(p => p.IsContentPack).ToArray();
            IModMetadata[] loadedMods = mods.Where(p => !p.IsContentPack).ToArray();
            //IModMetadata[] loadedContentPacks = this.GetAll(assemblyMods: false).ToArray();
            //IModMetadata[] loadedMods = this.GetAll(contentPacks: false).ToArray();

            // unlock content packs
            this.AreAllModsLoaded = true;

            // log loaded mods
            this.Monitor.Log($"Loaded {loadedMods.Length} mods" + (loadedMods.Length > 0 ? ":" : "."), LogLevel.Info);
            foreach (IModMetadata metadata in loadedMods.OrderBy(p => p.DisplayName))
            {
                IManifest manifest = metadata.Manifest;
                this.Monitor.Log(
                    $"   {metadata.DisplayName} {manifest.Version}"
                    + (!string.IsNullOrWhiteSpace(manifest.Author) ? $" by {manifest.Author}" : "")
                    + (!string.IsNullOrWhiteSpace(manifest.Description) ? $" | {manifest.Description}" : ""),
                    LogLevel.Info
                );
            }
            this.Monitor.Newline();

            // log loaded content packs
            if (loadedContentPacks.Any())
            {
                string GetModDisplayName(string id) => loadedMods.FirstOrDefault(p => p.HasID(id))?.DisplayName;

                this.Monitor.Log($"Loaded {loadedContentPacks.Length} content packs:", LogLevel.Info);
                foreach (IModMetadata metadata in loadedContentPacks.OrderBy(p => p.DisplayName))
                {
                    IManifest manifest = metadata.Manifest;
                    this.Monitor.Log(
                        $"   {metadata.DisplayName} {manifest.Version}"
                        + (!string.IsNullOrWhiteSpace(manifest.Author) ? $" by {manifest.Author}" : "")
                        + (metadata.IsContentPack ? $" | for {GetModDisplayName(metadata.Manifest.ContentPackFor.UniqueID)}" : "")
                        + (!string.IsNullOrWhiteSpace(manifest.Description) ? $" | {manifest.Description}" : ""),
                        LogLevel.Info
                    );
                }
                this.Monitor.Newline();
            }

            // log mod warnings
            this.LogModWarnings(mods.ToArray(), skippedMods);

            if (loadedMods.Length == 0)
            {
                // unlock mod integrations
                this.AreAllModsInitialised = true;
                return;
            }

            // initialise translations
            this.ReloadTranslations(loadedMods);

            // initialise loaded non-content-pack mods
            foreach (IModMetadata metadata in loadedMods)
            {
                // add interceptors
                if (metadata.Mod.Helper.Content is ContentHelper helper)
                {
                    // ReSharper disable SuspiciousTypeConversion.Global
                    if (metadata.Mod is IAssetEditor editor)
                        helper.ObservableAssetEditors.Add(editor);
                    if (metadata.Mod is IAssetLoader loader)
                        helper.ObservableAssetLoaders.Add(loader);
                    // ReSharper restore SuspiciousTypeConversion.Global

                    this.ContentCore.Editors[metadata] = helper.ObservableAssetEditors;
                    this.ContentCore.Loaders[metadata] = helper.ObservableAssetLoaders;
                }

                // call entry method
                try
                {
                    IMod mod = metadata.Mod;
                    mod.Entry(mod.Helper);
                }
                catch (Exception ex)
                {
                    metadata.LogAsMod($"Mod crashed on entry and might not work correctly. Technical details:\n{ex.GetLogSummary()}", LogLevel.Error);
                }

                // get mod API
                try
                {
                    object api = metadata.Mod.GetApi();
                    if (api != null && !api.GetType().IsPublic)
                    {
                        api = null;
                        this.Monitor.Log($"{metadata.DisplayName} provides an API instance with a non-public type. This isn't currently supported, so the API won't be available to other mods.", LogLevel.Warn);
                    }

                    if (api != null)
                        this.Monitor.Log($"   Found mod-provided API ({api.GetType().FullName}).", LogLevel.Trace);
                    metadata.SetApi(api);
                }
                catch (Exception ex)
                {
                    this.Monitor.Log($"Failed loading mod-provided API for {metadata.DisplayName}. Integrations with other mods may not work. Error: {ex.GetLogSummary()}", LogLevel.Error);
                }
            }

            // invalidate cache entries when needed
            // (These listeners are registered after Entry to avoid repeatedly reloading assets as mods initialise.)
            foreach (IModMetadata metadata in loadedMods)
            {
                if (metadata.Mod.Helper.Content is ContentHelper helper)
                {
                    helper.ObservableAssetEditors.CollectionChanged += (sender, e) =>
                    {
                        if (e.NewItems?.Count > 0)
                        {
                            this.Monitor.Log("Invalidating cache entries for new asset editors...", LogLevel.Trace);
                            this.ContentCore.InvalidateCacheFor(e.NewItems.Cast<IAssetEditor>().ToArray(), new IAssetLoader[0]);
                        }
                    };
                    helper.ObservableAssetLoaders.CollectionChanged += (sender, e) =>
                    {
                        if (e.NewItems?.Count > 0)
                        {
                            this.Monitor.Log("Invalidating cache entries for new asset loaders...", LogLevel.Trace);
                            this.ContentCore.InvalidateCacheFor(new IAssetEditor[0], e.NewItems.Cast<IAssetLoader>().ToArray());
                        }
                    };
                }
            }

            // reset cache now if any editors or loaders were added during entry
            IAssetEditor[] editors = loadedMods.SelectMany(p => p.Mod.Helper.Content.AssetEditors).ToArray();
            IAssetLoader[] loaders = loadedMods.SelectMany(p => p.Mod.Helper.Content.AssetLoaders).ToArray();
            if (editors.Any() || loaders.Any())
            {
                this.Monitor.Log("Invalidating cached assets for new editors & loaders...", LogLevel.Trace);
                this.ContentCore.InvalidateCacheFor(editors, loaders);
            }

            // unlock mod integrations
            this.AreAllModsInitialised = true;
        }

        /// <summary>Load a given mod.</summary>
        /// <param name="modsPath">The source path from where this mod is being loaded.</param>
        /// <param name="mod">The mod to load.</param>
        /// <param name="mods">The mods being loaded.</param>
        /// <param name="assemblyLoader">Preprocesses and loads mod assemblies</param>
        /// <param name="proxyFactory">Generates proxy classes to access mod APIs through an arbitrary interface.</param>
        /// <param name="suppressUpdateChecks">The mod IDs to ignore when validating update keys.</param>
        /// <param name="errorReasonPhrase">The user-facing reason phrase explaining why the mod couldn't be loaded (if applicable).</param>
        /// <param name="errorDetails">More detailed details about the error intended for developers (if any).</param>
        /// <returns>Returns whether the mod was successfully loaded.</returns>
        private bool TryLoadMod(string modsPath, IModMetadata mod, IModMetadata[] mods, AssemblyLoader assemblyLoader, InterfaceProxyFactory proxyFactory, HashSet<string> suppressUpdateChecks, out string errorReasonPhrase, out string errorDetails)
        {
            errorDetails = null;

            // log entry
            {
                string relativePath = PathUtilities.GetRelativePath(modsPath, mod.DirectoryPath);

                if (mod.IsContentPack)
                    this.Monitor.Log($"   {mod.DisplayName} ({relativePath}) [content pack]...", LogLevel.Trace);
                else if (mod.Manifest?.EntryDll != null)
                    this.Monitor.Log($"   {mod.DisplayName} ({relativePath}{Path.DirectorySeparatorChar}{mod.Manifest.EntryDll})...", LogLevel.Trace); // don't use Path.Combine here, since EntryDLL might not be valid
                else
                    this.Monitor.Log($"   {mod.DisplayName} ({relativePath})...", LogLevel.Trace);
            }

            // add warning for missing update key
            if (mod.HasID() && !suppressUpdateChecks.Contains(mod.Manifest.UniqueID) && !mod.HasValidUpdateKeys())
                mod.SetWarning(ModWarning.NoUpdateKeys);

            // validate status
            if (mod.Status == ModMetadataStatus.Failed)
            {
                this.Monitor.Log($"      Failed: {mod.Error}", LogLevel.Trace);
                errorReasonPhrase = mod.Error;
                return false;
            }

            // validate dependencies
            // Although dependences are validated before mods are loaded, a dependency may have failed to load.
            if (mod.Manifest.Dependencies?.Any() == true)
            {
                foreach (IManifestDependency dependency in mod.Manifest.Dependencies.Where(p => p.IsRequired))
                {
                    if (this.Get(dependency.UniqueID) == null)
                    {
                        string dependencyName = mods
                            .FirstOrDefault(otherMod => otherMod.HasID(dependency.UniqueID))
                            ?.DisplayName ?? dependency.UniqueID;
                        errorReasonPhrase = $"it needs the '{dependencyName}' mod, which couldn't be loaded.";
                        return false;
                    }
                }
            }

            // load as content pack
            if (mod.IsContentPack)
            {
                IManifest manifest = mod.Manifest;
                IMonitor monitor = this.Core.GetSecondaryMonitor(mod.DisplayName);
                IContentHelper contentHelper = new ContentHelper(this.ContentCore, mod.DirectoryPath, manifest.UniqueID, mod.DisplayName, monitor);
                IContentPack contentPack = new ContentPack(mod.DirectoryPath, manifest, contentHelper, this.JsonHelper);
                mod.SetMod(contentPack, monitor);
                this.Add(mod);

                errorReasonPhrase = null;
                return true;
            }

            // load as mod
            else
            {
                IManifest manifest = mod.Manifest;

                // load mod
                string assemblyPath = manifest?.EntryDll != null
                    ? Path.Combine(mod.DirectoryPath, manifest.EntryDll)
                    : null;
                Assembly modAssembly;
                try
                {
                    modAssembly = assemblyLoader.Load(mod, assemblyPath, assumeCompatible: mod.DataRecord?.Status == ModStatus.AssumeCompatible);
                    this.TrackAssemblies(mod, modAssembly);
                }
                catch (IncompatibleInstructionException) // details already in trace logs
                {
                    string[] updateUrls = new[] { this.ModDatabase.GetModPageUrlFor(manifest.UniqueID), "https://mods.smapi.io" }.Where(p => p != null).ToArray();
                    errorReasonPhrase = $"it's no longer compatible. Please check for a new version at {string.Join(" or ", updateUrls)}";
                    return false;
                }
                catch (SAssemblyLoadFailedException ex)
                {
                    errorReasonPhrase = $"it DLL couldn't be loaded: {ex.Message}";
                    return false;
                }
                catch (Exception ex)
                {
                    errorReasonPhrase = "its DLL couldn't be loaded.";
                    errorDetails = $"Error: {ex.GetLogSummary()}";
                    return false;
                }

                // initialise mod
                try
                {
                    // get mod instance
                    if (!this.TryLoadModEntry(modAssembly, out Mod modEntry, out errorReasonPhrase))
                        return false;

                    // get content packs
                    IContentPack[] GetContentPacks()
                    {
                        if (!this.AreAllModsLoaded)
                            throw new InvalidOperationException("Can't access content packs before SMAPI finishes loading mods.");

                        return this
                            .GetAll(assemblyMods: false)
                            .Where(p => p.IsContentPack && mod.HasID(p.Manifest.ContentPackFor.UniqueID))
                            .Select(p => p.ContentPack)
                            .ToArray();
                    }

                    // init mod helpers
                    IMonitor monitor = this.Core.GetSecondaryMonitor(mod.DisplayName);

                    IModHelper modHelper;
                    {
                        IModEvents events = new ModEvents(mod, this.EventManager);
                        ICommandHelper commandHelper = new CommandHelper(mod, this.GameInstance.CommandManager);
                        IContentHelper contentHelper = new ContentHelper(this.ContentCore, mod.DirectoryPath, manifest.UniqueID, mod.DisplayName, monitor);
                        IContentPackHelper contentPackHelper = new ContentPackHelper(manifest.UniqueID, new Lazy<IContentPack[]>(GetContentPacks), CreateFakeContentPack);
                        IDataHelper dataHelper = new DataHelper(manifest.UniqueID, mod.DirectoryPath, this.JsonHelper);
                        IReflectionHelper reflectionHelper = new ReflectionHelper(manifest.UniqueID, mod.DisplayName, this.Reflection);
                        IModRegistry modRegistryHelper = new ModRegistryHelper(manifest.UniqueID, this, proxyFactory, monitor);
                        IMultiplayerHelper multiplayerHelper = new MultiplayerHelper(manifest.UniqueID, this.GameInstance.Multiplayer);
                        ITranslationHelper translationHelper = new TranslationHelper(manifest.UniqueID, manifest.Name, this.ContentCore.GetLocale(), this.ContentCore.Language);
                        IContentPack CreateFakeContentPack(string packDirPath, IManifest packManifest)
                        {
                            IMonitor packMonitor = this.Core.GetSecondaryMonitor(packManifest.Name);
                            IContentHelper packContentHelper = new ContentHelper(this.ContentCore, packDirPath, packManifest.UniqueID, packManifest.Name, packMonitor);
                            return new ContentPack(packDirPath, packManifest, packContentHelper, this.JsonHelper);
                        }
                        modHelper = new ModHelper(manifest.UniqueID, mod.DirectoryPath, this.GameInstance.Input, events, contentHelper, contentPackHelper, commandHelper, dataHelper, modRegistryHelper, reflectionHelper, multiplayerHelper, translationHelper);
                    }

                    // init mod
                    modEntry.ModManifest = manifest;
                    modEntry.Helper = modHelper;
                    modEntry.Monitor = monitor;

                    // track mod
                    mod.SetMod(modEntry);
                    this.Add(mod);

                    return true;
                }
                catch (Exception ex)
                {
                    errorReasonPhrase = $"initialisation failed:\n{ex.GetLogSummary()}";
                    return false;
                }
            }
        }

        /// <summary>Load a mod's entry class.</summary>
        /// <param name="modAssembly">The mod assembly.</param>
        /// <param name="mod">The loaded instance.</param>
        /// <param name="error">The error indicating why loading failed (if applicable).</param>
        /// <returns>Returns whether the mod entry class was successfully loaded.</returns>
        private bool TryLoadModEntry(Assembly modAssembly, out Mod mod, out string error)
        {
            mod = null;

            // find type
            TypeInfo[] modEntries = modAssembly.DefinedTypes.Where(type => typeof(Mod).IsAssignableFrom(type) && !type.IsAbstract).Take(2).ToArray();
            if (modEntries.Length == 0)
            {
                error = $"its DLL has no '{nameof(Mod)}' subclass.";
                return false;
            }
            if (modEntries.Length > 1)
            {
                error = $"its DLL contains multiple '{nameof(Mod)}' subclasses.";
                return false;
            }

            // get implementation
            mod = (Mod)modAssembly.CreateInstance(modEntries[0].ToString());
            if (mod == null)
            {
                error = "its entry class couldn't be instantiated.";
                return false;
            }

            error = null;
            return true;
        }

        /// <summary>Write a summary of mod warnings to the console and log.</summary>
        /// <param name="mods">The loaded mods.</param>
        /// <param name="skippedMods">The mods which were skipped, along with the friendly and developer reasons.</param>
        private void LogModWarnings(IModMetadata[] mods, IDictionary<IModMetadata, Tuple<string, string>> skippedMods)
        {
            // get mods with warnings
            IModMetadata[] modsWithWarnings = mods.Where(p => p.Warnings != ModWarning.None).ToArray();
            if (!modsWithWarnings.Any() && !skippedMods.Any())
                return;

            // log intro
            {
                int count = modsWithWarnings.Union(skippedMods.Keys).Count();
                this.Monitor.Log($"Found {count} mod{(count == 1 ? "" : "s")} with warnings:", LogLevel.Info);
            }

            // log skipped mods
            if (skippedMods.Any())
            {
                this.Monitor.Log("   Skipped mods", LogLevel.Error);
                this.Monitor.Log("   " + "".PadRight(50, '-'), LogLevel.Error);
                this.Monitor.Log("      These mods could not be added to your game.", LogLevel.Error);
                this.Monitor.Newline();

                HashSet<string> logged = new HashSet<string>();
                foreach (var pair in skippedMods.OrderBy(p => p.Key.DisplayName))
                {
                    IModMetadata mod = pair.Key;
                    string errorReason = pair.Value.Item1;
                    string errorDetails = pair.Value.Item2;
                    string message = $"      - {mod.DisplayName}{(mod.Manifest?.Version != null ? " " + mod.Manifest.Version.ToString() : "")} because {errorReason}";

                    if (!logged.Add($"{message}|{errorDetails}"))
                        continue; // skip duplicate messages (e.g. if multiple copies of the mod are installed)

                    this.Monitor.Log(message, LogLevel.Error);
                    if (errorDetails != null)
                        this.Monitor.Log($"        ({errorDetails})", LogLevel.Trace);
                }
                this.Monitor.Newline();
            }

            // log warnings
            if (modsWithWarnings.Any())
            {
                // issue block format logic
                void LogWarningGroup(ModWarning warning, LogLevel logLevel, string heading, params string[] blurb)
                {
                    IModMetadata[] matches = modsWithWarnings
                        .Where(mod => mod.HasUnsuppressWarning(warning))
                        .ToArray();
                    if (!matches.Any())
                        return;

                    this.Monitor.Log("   " + heading, logLevel);
                    this.Monitor.Log("   " + "".PadRight(50, '-'), logLevel);
                    foreach (string line in blurb)
                        this.Monitor.Log("      " + line, logLevel);
                    this.Monitor.Newline();
                    foreach (IModMetadata match in matches)
                        this.Monitor.Log($"      - {match.DisplayName}", logLevel);
                    this.Monitor.Newline();
                }

                // supported issues
                LogWarningGroup(ModWarning.BrokenCodeLoaded, LogLevel.Error, "Broken mods",
                    "These mods have broken code, but you configured SMAPI to load them anyway. This may cause bugs,",
                    "errors, or crashes in-game."
                );
                LogWarningGroup(ModWarning.ChangesSaveSerialiser, LogLevel.Warn, "Changed save serialiser",
                    "These mods change the save serialiser. They may corrupt your save files, or make them unusable if",
                    "you uninstall these mods."
                );
                if (this.Settings.ParanoidWarnings)
                {
                    LogWarningGroup(ModWarning.AccessesFilesystem, LogLevel.Warn, "Accesses filesystem directly",
                        "These mods directly access the filesystem, and you enabled paranoid warnings. (Note that this may be",
                        "legitimate and innocent usage; this warning is meaningless without further investigation.)"
                    );
                    LogWarningGroup(ModWarning.AccessesShell, LogLevel.Warn, "Accesses shell/process directly",
                        "These mods directly access the OS shell or processes, and you enabled paranoid warnings. (Note that",
                        "this may be legitimate and innocent usage; this warning is meaningless without further investigation.)"
                    );
                }
                LogWarningGroup(ModWarning.PatchesGame, LogLevel.Info, "Patched game code",
                    "These mods directly change the game code. They're more likely to cause errors or bugs in-game; if",
                    "your game has issues, try removing these first. Otherwise you can ignore this warning."
                );
                LogWarningGroup(ModWarning.UsesUnvalidatedUpdateTick, LogLevel.Info, "Bypassed safety checks",
                    "These mods bypass SMAPI's normal safety checks, so they're more likely to cause errors or save",
                    "corruption. If your game has issues, try removing these first."
                );
                LogWarningGroup(ModWarning.NoUpdateKeys, LogLevel.Debug, "No update keys",
                    "These mods have no update keys in their manifest. SMAPI may not notify you about updates for these",
                    "mods. Consider notifying the mod authors about this problem."
                );
                LogWarningGroup(ModWarning.UsesDynamic, LogLevel.Debug, "Not crossplatform",
                    "These mods use the 'dynamic' keyword, and won't work on Linux/Mac."
                );
            }
        }
    }
}
