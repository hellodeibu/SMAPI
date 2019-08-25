using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Net;
using System.Runtime.ExceptionServices;
using System.Security;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
#if SMAPI_FOR_WINDOWS
using System.Windows.Forms;
#endif
using Newtonsoft.Json;
using StardewModdingAPI.Framework.Logging;
using StardewModdingAPI.Framework.Models;
using StardewModdingAPI.Framework.ModHelpers;
using StardewModdingAPI.Framework.ModLoading;
using StardewModdingAPI.Framework.Patching;
using StardewModdingAPI.Framework.Reflection;
using StardewModdingAPI.Framework.Serialisation;
using StardewModdingAPI.Internal;
using StardewModdingAPI.Patches;
using StardewModdingAPI.Toolkit;
using StardewModdingAPI.Toolkit.Framework.Clients.WebApi;
using StardewModdingAPI.Toolkit.Utilities;
using StardewValley;
using Object = StardewValley.Object;
using ThreadState = System.Threading.ThreadState;

namespace StardewModdingAPI.Framework
{
    /// <summary>The core class which initialises and manages SMAPI.</summary>
    internal class SCore : IDisposable
    {
        /*********
        ** Fields
        *********/
        /// <summary>The log file to which to write messages.</summary>
        private readonly LogFileManager LogFile;

        /// <summary>Manages console output interception.</summary>
        private readonly ConsoleInterceptionManager ConsoleManager = new ConsoleInterceptionManager();

        /// <summary>The core logger and monitor for SMAPI.</summary>
        private readonly Monitor Monitor;

        /// <summary>The core logger and monitor on behalf of the game.</summary>
        private readonly Monitor MonitorForGame;

        /// <summary>Tracks whether the game should exit immediately and any pending initialisation should be cancelled.</summary>
        private readonly CancellationTokenSource CancellationTokenSource = new CancellationTokenSource();

        /// <summary>Simplifies access to private game code.</summary>
        private readonly Reflector Reflection = new Reflector();

        /// <summary>The SMAPI configuration settings.</summary>
        private readonly SConfig Settings;

        /// <summary>The underlying game instance.</summary>
        public SGame GameInstance;

        /// <summary>The underlying content manager.</summary>
        private ContentCoordinator ContentCore => this.GameInstance.ContentCore;

        /// <summary>Tracks the installed mods.</summary>
        /// <remarks>This is initialised after the game starts.</remarks>
        private readonly ModRegistry ModRegistry;

        /// <summary>Whether the game is currently running.</summary>
        private bool IsGameRunning;

        /// <summary>Whether the program has been disposed.</summary>
        private bool IsDisposed;

        /// <summary>Regex patterns which match console messages to suppress from the console and log.</summary>
        private readonly Regex[] SuppressConsolePatterns =
        {
            new Regex(@"^TextBox\.Selected is now '(?:True|False)'\.$", RegexOptions.Compiled | RegexOptions.CultureInvariant),
            new Regex(@"^(?:FRUIT )?TREE: IsClient:(?:True|False) randomOutput: \d+$", RegexOptions.Compiled | RegexOptions.CultureInvariant),
            new Regex(@"^loadPreferences\(\); begin", RegexOptions.Compiled | RegexOptions.CultureInvariant),
            new Regex(@"^savePreferences\(\); async=", RegexOptions.Compiled | RegexOptions.CultureInvariant),
            new Regex(@"^DebugOutput:\s+(?:added CLOUD|added cricket|dismount tile|Ping|playerPos)", RegexOptions.Compiled | RegexOptions.CultureInvariant),
            new Regex(@"^static SerializableDictionary<.+>\(\) called\.$", RegexOptions.Compiled | RegexOptions.CultureInvariant),
        };

        /// <summary>Regex patterns which match console messages to show a more friendly error for.</summary>
        private readonly Tuple<Regex, string, LogLevel>[] ReplaceConsolePatterns =
        {
            Tuple.Create(
                new Regex(@"^System\.InvalidOperationException: Steamworks is not initialized\.", RegexOptions.Compiled | RegexOptions.CultureInvariant),
#if SMAPI_FOR_WINDOWS
                "Oops! Steam achievements won't work because Steam isn't loaded. You can launch the game through Steam to fix that (see 'Part 2: Configure Steam' in the install guide for more info: https://smapi.io/install).",
#else
                "Oops! Steam achievements won't work because Steam isn't loaded. You can launch the game through Steam to fix that.",
#endif
                LogLevel.Error
            )
        };

        /// <summary>The mod toolkit used for generic mod interactions.</summary>
        private readonly ModToolkit Toolkit = new ModToolkit();

        /// <summary>The path to search for mods.</summary>
        private string ModsPath => Constants.ModsPath;


        /*********
        ** Accessors
        *********/
        /// <summary>Manages deprecation warnings.</summary>
        /// <remarks>This is initialised after the game starts. This is accessed directly because it's not part of the normal class model.</remarks>
        internal static DeprecationManager DeprecationManager { get; private set; }


        /*********
        ** Public methods
        *********/
        /// <summary>Construct an instance.</summary>
        /// <param name="modsPath">The path to search for mods.</param>
        /// <param name="writeToConsole">Whether to output log messages to the console.</param>
        /// <param name="interactiveConsole">Whether to enable interactive console mode.</param>
        public SCore(string modsPath, bool writeToConsole, bool interactiveConsole = true)
        {
            // init paths
            this.VerifyPath(modsPath);
            this.VerifyPath(Constants.LogDir);
            Constants.ModsPath = modsPath;

            // init log file
            this.PurgeNormalLogs();
            string logPath = this.GetLogPath();

            // init basics
            this.Settings = JsonConvert.DeserializeObject<SConfig>(File.ReadAllText(Constants.ApiConfigPath));
            this.LogFile = new LogFileManager(logPath);
            this.Monitor = new Monitor("SMAPI", this.ConsoleManager, this.LogFile, this.CancellationTokenSource, this.Settings.ColorScheme, this.Settings.VerboseLogging)
            {
                WriteToConsole = writeToConsole,
                InteractiveConsole = interactiveConsole,
                ShowTraceInConsole = this.Settings.DeveloperMode,
                ShowFullStampInConsole = this.Settings.DeveloperMode
            };
            this.MonitorForGame = this.GetSecondaryMonitor("game");

            // redirect direct console output
            if (this.MonitorForGame.WriteToConsole)
                this.ConsoleManager.OnMessageIntercepted += message => this.HandleConsoleMessage(this.MonitorForGame, message);

            // init logging
            this.Monitor.Log($"SMAPI {Constants.ApiVersion} with Stardew Valley {Constants.GameVersion} on {EnvironmentUtility.GetFriendlyPlatformName(Constants.Platform)}", LogLevel.Info);
            this.Monitor.Log($"Mods go here: {modsPath}");
            if (modsPath != Constants.DefaultModsPath)
                this.Monitor.Log("(Using custom --mods-path argument.)", LogLevel.Trace);
            this.Monitor.Log($"Log started at {DateTime.UtcNow:s} UTC", LogLevel.Trace);

            // init ModRegistry
            this.ModRegistry = new ModRegistry(
                this,
                this.Monitor,
                this.Toolkit.JsonHelper,
                this.Reflection,
                this.Settings);

            // Init deprecation manager
            SCore.DeprecationManager = new DeprecationManager(this.Monitor, this.ModRegistry);

            // validate platform
#if SMAPI_FOR_WINDOWS
            if (Constants.Platform != Platform.Windows)
            {
                this.Monitor.Log("Oops! You're running Windows, but this version of SMAPI is for Linux or Mac. Please reinstall SMAPI to fix this.", LogLevel.Error);
                this.PressAnyKeyToExit();
                return;
            }
#else
            if (Constants.Platform == Platform.Windows)
            {
                this.Monitor.Log("Oops! You're running {Constants.Platform}, but this version of SMAPI is for Windows. Please reinstall SMAPI to fix this.", LogLevel.Error);
                this.PressAnyKeyToExit();
                return;
            }
#endif
        }

        /// <summary>Launch SMAPI.</summary>
        [HandleProcessCorruptedStateExceptions, SecurityCritical] // let try..catch handle corrupted state exceptions
        public void RunInteractively()
        {
            // initialise SMAPI
            try
            {
                JsonConverter[] converters = {
                    new ColorConverter(),
                    new PointConverter(),
                    new RectangleConverter()
                };
                foreach (JsonConverter converter in converters)
                    this.Toolkit.JsonHelper.JsonSettings.Converters.Add(converter);

                // add error handlers
#if SMAPI_FOR_WINDOWS
                Application.ThreadException += (sender, e) => this.Monitor.Log($"Critical thread exception: {e.Exception.GetLogSummary()}", LogLevel.Error);
                Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException);
#endif
                AppDomain.CurrentDomain.UnhandledException += (sender, e) => this.Monitor.Log($"Critical app domain exception: {e.ExceptionObject}", LogLevel.Error);

                // add more leniant assembly resolvers
                AppDomain.CurrentDomain.AssemblyResolve += (sender, e) => AssemblyLoader.ResolveAssembly(e.Name);

                // override game
                SGame.ConstructorHack = new SGameConstructorHack(this.Monitor, this.Reflection, this.Toolkit.JsonHelper);
                this.GameInstance = new SGame(
                    this.Monitor,
                    this.MonitorForGame,
                    this.Reflection,
                    this.ModRegistry.EventManager,
                    this.Toolkit.JsonHelper,
                    this.ModRegistry,
                    SCore.DeprecationManager,
                    this.OnLocaleChanged,
                    this.InitialiseAfterGameStart,
                    this.Dispose
                );
                StardewValley.Program.gamePtr = this.GameInstance;

                // apply game patches
                new GamePatcher(this.Monitor).Apply(
                    new DialogueErrorPatch(this.MonitorForGame, this.Reflection),
                    new ObjectErrorPatch(),
                    new LoadForNewGamePatch(this.Reflection, this.GameInstance.OnLoadStageChanged)
                );

                // add exit handler
                new Thread(() =>
                {
                    this.CancellationTokenSource.Token.WaitHandle.WaitOne();
                    if (this.IsGameRunning)
                    {
                        try
                        {
                            File.WriteAllText(Constants.FatalCrashMarker, string.Empty);
                            File.Copy(this.LogFile.Path, Constants.FatalCrashLog, overwrite: true);
                        }
                        catch (Exception ex)
                        {
                            this.Monitor.Log($"SMAPI failed trying to track the crash details: {ex.GetLogSummary()}");
                        }

                        this.GameInstance.Exit();
                    }
                }).Start();

                // set window titles
                this.UpdateWindowTitles();
            }
            catch (Exception ex)
            {
                this.Monitor.Log($"SMAPI failed to initialise: {ex.GetLogSummary()}", LogLevel.Error);
                this.PressAnyKeyToExit();
                return;
            }

            // check update marker
            if (File.Exists(Constants.UpdateMarker))
            {
                string rawUpdateFound = File.ReadAllText(Constants.UpdateMarker);
                if (SemanticVersion.TryParse(rawUpdateFound, out ISemanticVersion updateFound))
                {
                    if (Constants.ApiVersion.IsPrerelease() && updateFound.IsNewerThan(Constants.ApiVersion))
                    {
                        this.Monitor.Log("A new version of SMAPI was detected last time you played.", LogLevel.Error);
                        this.Monitor.Log($"You can update to {updateFound}: https://smapi.io.", LogLevel.Error);

                        if (this.Monitor.InteractiveConsole)
                        {
                            this.Monitor.Log("Press any key to continue playing anyway. (This only appears when using a SMAPI beta.)", LogLevel.Info);
                            Console.ReadKey();
                        }
                    }
                }
                File.Delete(Constants.UpdateMarker);
            }

            // show details if game crashed during last session
            if (File.Exists(Constants.FatalCrashMarker))
            {
                this.Monitor.Log("The game crashed last time you played. That can be due to bugs in the game, but if it happens repeatedly you can ask for help here: https://community.playstarbound.com/threads/108375/.", LogLevel.Error);
                this.Monitor.Log("If you ask for help, make sure to share your SMAPI log: https://log.smapi.io.", LogLevel.Error);

                if (this.Monitor.InteractiveConsole)
                {
                    this.Monitor.Log("Press any key to delete the crash data and continue playing.", LogLevel.Info);
                    Console.ReadKey();
                }

                File.Delete(Constants.FatalCrashLog);
                File.Delete(Constants.FatalCrashMarker);
            }

            // start game
            this.Monitor.Log("Starting game...", LogLevel.Debug);
            try
            {
                this.IsGameRunning = true;
                StardewValley.Program.releaseBuild = true; // game's debug logic interferes with SMAPI opening the game window
                this.GameInstance.Run();
            }
            catch (InvalidOperationException ex) when (ex.Source == "Microsoft.Xna.Framework.Xact" && ex.StackTrace.Contains("Microsoft.Xna.Framework.Audio.AudioEngine..ctor"))
            {
                this.Monitor.Log("The game couldn't load audio. Do you have speakers or headphones plugged in?", LogLevel.Error);
                this.Monitor.Log($"Technical details: {ex.GetLogSummary()}", LogLevel.Trace);
                this.PressAnyKeyToExit();
            }
            catch (FileNotFoundException ex) when (ex.Message == "Could not find file 'C:\\Program Files (x86)\\Steam\\SteamApps\\common\\Stardew Valley\\Content\\XACT\\FarmerSounds.xgs'.") // path in error is hardcoded regardless of install path
            {
                this.Monitor.Log("The game can't find its Content\\XACT\\FarmerSounds.xgs file. You can usually fix this by resetting your content files (see https://smapi.io/troubleshoot#reset-content ), or by uninstalling and reinstalling the game.", LogLevel.Error);
                this.Monitor.Log($"Technical details: {ex.GetLogSummary()}", LogLevel.Trace);
                this.PressAnyKeyToExit();
            }
            catch (Exception ex)
            {
                this.MonitorForGame.Log($"The game failed to launch: {ex.GetLogSummary()}", LogLevel.Error);
                this.PressAnyKeyToExit();
            }
            finally
            {
                this.Dispose();
            }
        }

        /// <summary>Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.</summary>
        public void Dispose()
        {
            // skip if already disposed
            if (this.IsDisposed)
                return;
            this.IsDisposed = true;
            this.Monitor.Log("Disposing...", LogLevel.Trace);

            // dispose mod data
            foreach (IModMetadata mod in this.ModRegistry.GetAll())
            {
                try
                {
                    (mod.Mod as IDisposable)?.Dispose();
                }
                catch (Exception ex)
                {
                    mod.LogAsMod($"Mod failed during disposal: {ex.GetLogSummary()}.", LogLevel.Warn);
                }
            }

            // dispose core components
            this.IsGameRunning = false;
            this.ConsoleManager?.Dispose();
            this.ContentCore?.Dispose();
            this.CancellationTokenSource?.Dispose();
            this.GameInstance?.Dispose();
            this.LogFile?.Dispose();

            // end game (moved from Game1.OnExiting to let us clean up first)
            Process.GetCurrentProcess().Kill();
        }

        /*********
        ** Private methods
        *********/
        /// <summary>Initialise SMAPI and mods after the game starts.</summary>
        private void InitialiseAfterGameStart()
        {
            // add headers
            if (this.Settings.DeveloperMode)
                this.Monitor.Log($"You have SMAPI for developers, so the console will be much more verbose. You can disable developer mode by installing the non-developer version of SMAPI, or by editing {Constants.ApiConfigPath}.", LogLevel.Info);
            if (!this.Settings.CheckForUpdates)
                this.Monitor.Log($"You configured SMAPI to not check for updates. Running an old version of SMAPI is not recommended. You can enable update checks by reinstalling SMAPI or editing {Constants.ApiConfigPath}.", LogLevel.Warn);
            if (!this.Monitor.WriteToConsole)
                this.Monitor.Log("Writing to the terminal is disabled because the --no-terminal argument was received. This usually means launching the terminal failed.", LogLevel.Warn);
            if (!this.Monitor.InteractiveConsole)
                this.Monitor.Log("Sending commands via the terminal is disabled because the --no-interactive argument was received.", LogLevel.Info);

            this.Monitor.VerboseLog("Verbose logging enabled.");

            // validate XNB integrity
            if (!this.ValidateContentIntegrity())
                this.Monitor.Log("SMAPI found problems in your game's content files which are likely to cause errors or crashes. Consider uninstalling XNB mods or reinstalling the game.", LogLevel.Error);

            // Load mods
            this.ModRegistry.LoadModsFromPath(this.ModsPath, (mods) =>
            {
                // write metadata file
                if (this.Settings.DumpMetadata)
                {
                    ModFolderExport export = new ModFolderExport
                    {
                        Exported = DateTime.UtcNow.ToString("O"),
                        ApiVersion = Constants.ApiVersion.ToString(),
                        GameVersion = Constants.GameVersion.ToString(),
                        ModFolderPath = ModsPath,
                        Mods = mods
                    };
                    this.Toolkit.JsonHelper.WriteJsonFile(Path.Combine(Constants.LogDir, $"{Constants.LogNamePrefix}metadata-dump.json"), export);
                }

                // check for updates
                this.CheckForUpdatesAsync(mods);
            });

            if (this.Monitor.IsExiting)
            {
                this.Monitor.Log("SMAPI shutting down: aborting initialisation.", LogLevel.Warn);
                return;
            }

            this.UpdateWindowTitles();

            // start SMAPI console
            if (this.Monitor.InteractiveConsole)
                new Thread(this.RunConsoleLoop).Start();
        }

        public void UpdateWindowTitles()
        {
            int modsLoaded = this.ModRegistry.GetAll().Count();
            string withMods = "";

            if (modsLoaded > 0)
                withMods = $" with {modsLoaded} mods";
                
            // update window titles
            this.GameInstance.Window.Title = $"Stardew Valley {Constants.GameVersion} - running SMAPI {Constants.ApiVersion}{withMods}";
            Console.Title = $"SMAPI {Constants.ApiVersion} - running Stardew Valley {Constants.GameVersion}{withMods}";
        }

        /// <summary>Handle the game changing locale.</summary>
        private void OnLocaleChanged()
        {
            // get locale
            string locale = this.ContentCore.GetLocale();
            LocalizedContentManager.LanguageCode languageCode = this.ContentCore.Language;

            // update mod translation helpers
            foreach (IModMetadata mod in this.ModRegistry.GetAll(contentPacks: false))
                (mod.Mod.Helper.Translation as TranslationHelper)?.SetLocale(locale, languageCode);
        }

        /// <summary>Run a loop handling console input.</summary>
        [SuppressMessage("ReSharper", "FunctionNeverReturns", Justification = "The thread is aborted when the game exits.")]
        private void RunConsoleLoop()
        {
            // prepare console
            this.Monitor.Log("Type 'help' for help, or 'help <cmd>' for a command's usage", LogLevel.Info);
            this.GameInstance.CommandManager.Add(null, "help", "Lists command documentation.\n\nUsage: help\nLists all available commands.\n\nUsage: help <cmd>\n- cmd: The name of a command whose documentation to display.", this.HandleCommand);
            this.GameInstance.CommandManager.Add(null, "reload_i18n", "Reloads translation files for all mods.\n\nUsage: reload_i18n", this.HandleCommand);

            // start handling command line input
            Thread inputThread = new Thread(() =>
            {
                while (true)
                {
                    // get input
                    string input = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(input))
                        continue;

                    // handle command
                    this.Monitor.LogUserInput(input);
                    this.GameInstance.CommandQueue.Enqueue(input);
                }
            });
            inputThread.Start();

            // keep console thread alive while the game is running
            while (this.IsGameRunning && !this.Monitor.IsExiting)
                Thread.Sleep(1000 / 10);
            if (inputThread.ThreadState == ThreadState.Running)
                inputThread.Abort();
        }

        /// <summary>Look for common issues with the game's XNB content, and log warnings if anything looks broken or outdated.</summary>
        /// <returns>Returns whether all integrity checks passed.</returns>
        private bool ValidateContentIntegrity()
        {
            this.Monitor.Log("Detecting common issues...", LogLevel.Trace);
            bool issuesFound = false;

            // object format (commonly broken by outdated files)
            {
                // detect issues
                bool hasObjectIssues = false;
                void LogIssue(int id, string issue) => this.Monitor.Log($@"Detected issue: item #{id} in Content\Data\ObjectInformation.xnb is invalid ({issue}).", LogLevel.Trace);
                foreach (KeyValuePair<int, string> entry in Game1.objectInformation)
                {
                    // must not be empty
                    if (string.IsNullOrWhiteSpace(entry.Value))
                    {
                        LogIssue(entry.Key, "entry is empty");
                        hasObjectIssues = true;
                        continue;
                    }

                    // require core fields
                    string[] fields = entry.Value.Split('/');
                    if (fields.Length < Object.objectInfoDescriptionIndex + 1)
                    {
                        LogIssue(entry.Key, "too few fields for an object");
                        hasObjectIssues = true;
                        continue;
                    }

                    // check min length for specific types
                    switch (fields[Object.objectInfoTypeIndex].Split(new[] { ' ' }, 2)[0])
                    {
                        case "Cooking":
                            if (fields.Length < Object.objectInfoBuffDurationIndex + 1)
                            {
                                LogIssue(entry.Key, "too few fields for a cooking item");
                                hasObjectIssues = true;
                            }
                            break;
                    }
                }

                // log error
                if (hasObjectIssues)
                {
                    issuesFound = true;
                    this.Monitor.Log(@"Your Content\Data\ObjectInformation.xnb file seems to be broken or outdated.", LogLevel.Warn);
                }
            }

            return !issuesFound;
        }

        /// <summary>Asynchronously check for a new version of SMAPI and any installed mods, and print alerts to the console if an update is available.</summary>
        /// <param name="mods">The mods to include in the update check (if eligible).</param>
        private void CheckForUpdatesAsync(IModMetadata[] mods)
        {
            if (!this.Settings.CheckForUpdates)
                return;

            new Thread(() =>
            {
                // create client
                string url = this.Settings.WebApiBaseUrl;
#if !SMAPI_FOR_WINDOWS
                url = url.Replace("https://", "http://"); // workaround for OpenSSL issues with the game's bundled Mono on Linux/Mac
#endif
                WebApiClient client = new WebApiClient(url, Constants.ApiVersion);
                this.Monitor.Log("Checking for updates...", LogLevel.Trace);

                // check SMAPI version
                ISemanticVersion updateFound = null;
                try
                {
                    ModEntryModel response = client.GetModInfo(new[] { new ModSearchEntryModel("Pathoschild.SMAPI", new[] { $"GitHub:{this.Settings.GitHubProjectName}" }) }).Single().Value;
                    ISemanticVersion latestStable = response.Main?.Version;
                    ISemanticVersion latestBeta = response.Optional?.Version;

                    if (latestStable == null && response.Errors.Any())
                    {
                        this.Monitor.Log("Couldn't check for a new version of SMAPI. This won't affect your game, but you may not be notified of new versions if this keeps happening.", LogLevel.Warn);
                        this.Monitor.Log($"Error: {string.Join("\n", response.Errors)}");
                    }
                    else if (this.IsValidUpdate(Constants.ApiVersion, latestBeta, this.Settings.UseBetaChannel))
                    {
                        updateFound = latestBeta;
                        this.Monitor.Log($"You can update SMAPI to {latestBeta}: {Constants.HomePageUrl}", LogLevel.Alert);
                    }
                    else if (this.IsValidUpdate(Constants.ApiVersion, latestStable, this.Settings.UseBetaChannel))
                    {
                        updateFound = latestStable;
                        this.Monitor.Log($"You can update SMAPI to {latestStable}: {Constants.HomePageUrl}", LogLevel.Alert);
                    }
                    else
                        this.Monitor.Log("   SMAPI okay.", LogLevel.Trace);
                }
                catch (Exception ex)
                {
                    this.Monitor.Log("Couldn't check for a new version of SMAPI. This won't affect your game, but you won't be notified of new versions if this keeps happening.", LogLevel.Warn);
                    this.Monitor.Log(ex is WebException && ex.InnerException == null
                        ? $"Error: {ex.Message}"
                        : $"Error: {ex.GetLogSummary()}"
                    );
                }

                // show update message on next launch
                if (updateFound != null)
                    File.WriteAllText(Constants.UpdateMarker, updateFound.ToString());

                // check mod versions
                if (mods.Any())
                {
                    try
                    {
                        HashSet<string> suppressUpdateChecks = new HashSet<string>(this.Settings.SuppressUpdateChecks, StringComparer.InvariantCultureIgnoreCase);

                        // prepare search model
                        List<ModSearchEntryModel> searchMods = new List<ModSearchEntryModel>();
                        foreach (IModMetadata mod in mods)
                        {
                            if (!mod.HasID() || suppressUpdateChecks.Contains(mod.Manifest.UniqueID))
                                continue;

                            string[] updateKeys = mod
                                .GetUpdateKeys(validOnly: true)
                                .Select(p => p.ToString())
                                .ToArray();
                            searchMods.Add(new ModSearchEntryModel(mod.Manifest.UniqueID, updateKeys.ToArray()));
                        }

                        // fetch results
                        this.Monitor.Log($"   Checking for updates to {searchMods.Count} mods...", LogLevel.Trace);
                        IDictionary<string, ModEntryModel> results = client.GetModInfo(searchMods.ToArray());

                        // extract update alerts & errors
                        var updates = new List<Tuple<IModMetadata, ISemanticVersion, string>>();
                        var errors = new StringBuilder();
                        foreach (IModMetadata mod in mods.OrderBy(p => p.DisplayName))
                        {
                            // link to update-check data
                            if (!mod.HasID() || !results.TryGetValue(mod.Manifest.UniqueID, out ModEntryModel result))
                                continue;
                            mod.SetUpdateData(result);

                            // handle errors
                            if (result.Errors != null && result.Errors.Any())
                            {
                                errors.AppendLine(result.Errors.Length == 1
                                    ? $"   {mod.DisplayName}: {result.Errors[0]}"
                                    : $"   {mod.DisplayName}:\n      - {string.Join("\n      - ", result.Errors)}"
                                );
                            }

                            // parse versions
                            bool useBetaInfo = result.HasBetaInfo && Constants.ApiVersion.IsPrerelease();
                            ISemanticVersion localVersion = mod.DataRecord?.GetLocalVersionForUpdateChecks(mod.Manifest.Version) ?? mod.Manifest.Version;
                            ISemanticVersion latestVersion = mod.DataRecord?.GetRemoteVersionForUpdateChecks(result.Main?.Version) ?? result.Main?.Version;
                            ISemanticVersion optionalVersion = mod.DataRecord?.GetRemoteVersionForUpdateChecks(result.Optional?.Version) ?? result.Optional?.Version;
                            ISemanticVersion unofficialVersion = useBetaInfo ? result.UnofficialForBeta?.Version : result.Unofficial?.Version;

                            // show update alerts
                            if (this.IsValidUpdate(localVersion, latestVersion, useBetaChannel: true))
                                updates.Add(Tuple.Create(mod, latestVersion, result.Main?.Url));
                            else if (this.IsValidUpdate(localVersion, optionalVersion, useBetaChannel: localVersion.IsPrerelease()))
                                updates.Add(Tuple.Create(mod, optionalVersion, result.Optional?.Url));
                            else if (this.IsValidUpdate(localVersion, unofficialVersion, useBetaChannel: mod.Status == ModMetadataStatus.Failed))
                                updates.Add(Tuple.Create(mod, unofficialVersion, useBetaInfo ? result.UnofficialForBeta?.Url : result.Unofficial?.Url));
                        }

                        // show update errors
                        if (errors.Length != 0)
                            this.Monitor.Log("Got update-check errors for some mods:\n" + errors.ToString().TrimEnd(), LogLevel.Trace);

                        // show update alerts
                        if (updates.Any())
                        {
                            this.Monitor.Newline();
                            this.Monitor.Log($"You can update {updates.Count} mod{(updates.Count != 1 ? "s" : "")}:", LogLevel.Alert);
                            foreach (var entry in updates)
                            {
                                IModMetadata mod = entry.Item1;
                                ISemanticVersion newVersion = entry.Item2;
                                string newUrl = entry.Item3;
                                this.Monitor.Log($"   {mod.DisplayName} {newVersion}: {newUrl}", LogLevel.Alert);
                            }
                        }
                        else
                            this.Monitor.Log("   All mods up to date.", LogLevel.Trace);
                    }
                    catch (Exception ex)
                    {
                        this.Monitor.Log("Couldn't check for new mod versions. This won't affect your game, but you won't be notified of mod updates if this keeps happening.", LogLevel.Warn);
                        this.Monitor.Log(ex is WebException && ex.InnerException == null
                            ? ex.Message
                            : ex.ToString()
                        );
                    }
                }
            }).Start();
        }

        /// <summary>Get whether a given version should be offered to the user as an update.</summary>
        /// <param name="currentVersion">The current semantic version.</param>
        /// <param name="newVersion">The target semantic version.</param>
        /// <param name="useBetaChannel">Whether the user enabled the beta channel and should be offered pre-release updates.</param>
        private bool IsValidUpdate(ISemanticVersion currentVersion, ISemanticVersion newVersion, bool useBetaChannel)
        {
            return
                newVersion != null
                && newVersion.IsNewerThan(currentVersion)
                && (useBetaChannel || !newVersion.IsPrerelease());
        }

        /// <summary>Create a directory path if it doesn't exist.</summary>
        /// <param name="path">The directory path.</param>
        private void VerifyPath(string path)
        {
            try
            {
                if (!Directory.Exists(path))
                    Directory.CreateDirectory(path);
            }
            catch (Exception ex)
            {
                // note: this happens before this.Monitor is initialised
                Console.WriteLine($"Couldn't create a path: {path}\n\n{ex.GetLogSummary()}");
            }
        }

        /// <summary>The method called when the user submits a core SMAPI command in the console.</summary>
        /// <param name="name">The command name.</param>
        /// <param name="arguments">The command arguments.</param>
        private void HandleCommand(string name, string[] arguments)
        {
            switch (name)
            {
                case "help":
                    if (arguments.Any())
                    {
                        Command result = this.GameInstance.CommandManager.Get(arguments[0]);
                        if (result == null)
                            this.Monitor.Log("There's no command with that name.", LogLevel.Error);
                        else
                            this.Monitor.Log($"{result.Name}: {result.Documentation}{(result.Mod != null ? $"\n(Added by {result.Mod.DisplayName}.)" : "")}", LogLevel.Info);
                    }
                    else
                    {
                        string message = "The following commands are registered:\n";
                        IGrouping<string, string>[] groups = (from command in this.GameInstance.CommandManager.GetAll() orderby command.Mod?.DisplayName, command.Name group command.Name by command.Mod?.DisplayName).ToArray();
                        foreach (var group in groups)
                        {
                            string modName = group.Key ?? "SMAPI";
                            string[] commandNames = group.ToArray();
                            message += $"{modName}:\n  {string.Join("\n  ", commandNames)}\n\n";
                        }
                        message += "For more information about a command, type 'help command_name'.";

                        this.Monitor.Log(message, LogLevel.Info);
                    }
                    break;

                case "reload_i18n":
                    this.ModRegistry.ReloadTranslations(this.ModRegistry.GetAll(contentPacks: false));
                    this.Monitor.Log("Reloaded translation files for all mods. This only affects new translations the mods fetch; if they cached some text, it may not be updated.", LogLevel.Info);
                    break;

                default:
                    throw new NotSupportedException($"Unrecognise core SMAPI command '{name}'.");
            }
        }

        /// <summary>Redirect messages logged directly to the console to the given monitor.</summary>
        /// <param name="gameMonitor">The monitor with which to log messages as the game.</param>
        /// <param name="message">The message to log.</param>
        private void HandleConsoleMessage(IMonitor gameMonitor, string message)
        {
            // detect exception
            LogLevel level = message.Contains("Exception") ? LogLevel.Error : LogLevel.Trace;

            // ignore suppressed message
            if (level != LogLevel.Error && this.SuppressConsolePatterns.Any(p => p.IsMatch(message)))
                return;

            // show friendly error if applicable
            foreach (var entry in this.ReplaceConsolePatterns)
            {
                if (entry.Item1.IsMatch(message))
                {
                    this.Monitor.Log(entry.Item2, entry.Item3);
                    gameMonitor.Log(message, LogLevel.Trace);
                    return;
                }
            }

            // forward to monitor
            gameMonitor.Log(message, level);
        }

        /// <summary>Show a 'press any key to exit' message, and exit when they press a key.</summary>
        private void PressAnyKeyToExit()
        {
            this.Monitor.Log("Game has ended. Press any key to exit.", LogLevel.Info);
            this.PressAnyKeyToExit(showMessage: false);
        }

        /// <summary>Show a 'press any key to exit' message, and exit when they press a key.</summary>
        /// <param name="showMessage">Whether to print a 'press any key to exit' message to the console.</param>
        private void PressAnyKeyToExit(bool showMessage)
        {
            if (showMessage)
                Console.WriteLine("Game has ended. Press any key to exit.");
            Thread.Sleep(100);
            if (this.Monitor.InteractiveConsole)
                Console.ReadKey();
            Environment.Exit(0);
        }

        /// <summary>Get a monitor instance derived from SMAPI's current settings.</summary>
        /// <param name="name">The name of the module which will log messages with this instance.</param>
        public Monitor GetSecondaryMonitor(string name)
        {
            return new Monitor(name, this.ConsoleManager, this.LogFile, this.CancellationTokenSource, this.Settings.ColorScheme, this.Settings.VerboseLogging)
            {
                WriteToConsole = this.Monitor.WriteToConsole,
                InteractiveConsole = this.Monitor.InteractiveConsole,
                ShowTraceInConsole = this.Settings.DeveloperMode,
                ShowFullStampInConsole = this.Settings.DeveloperMode
            };
        }

        /// <summary>Get the absolute path to the next available log file.</summary>
        private string GetLogPath()
        {
            // default path
            {
                FileInfo defaultFile = new FileInfo(Path.Combine(Constants.LogDir, $"{Constants.LogFilename}.{Constants.LogExtension}"));
                if (!defaultFile.Exists)
                    return defaultFile.FullName;
            }

            // get first disambiguated path
            for (int i = 2; i < int.MaxValue; i++)
            {
                FileInfo file = new FileInfo(Path.Combine(Constants.LogDir, $"{Constants.LogFilename}.player-{i}.{Constants.LogExtension}"));
                if (!file.Exists)
                    return file.FullName;
            }

            // should never happen
            throw new InvalidOperationException("Could not find an available log path.");
        }

        /// <summary>Delete normal (non-crash) log files created by SMAPI.</summary>
        private void PurgeNormalLogs()
        {
            DirectoryInfo logsDir = new DirectoryInfo(Constants.LogDir);
            if (!logsDir.Exists)
                return;

            foreach (FileInfo logFile in logsDir.EnumerateFiles())
            {
                // skip non-SMAPI file
                if (!logFile.Name.StartsWith(Constants.LogNamePrefix, StringComparison.InvariantCultureIgnoreCase))
                    continue;

                // skip crash log
                if (logFile.FullName == Constants.FatalCrashLog)
                    continue;

                // delete file
                try
                {
                    FileUtilities.ForceDelete(logFile);
                }
                catch (IOException)
                {
                    // ignore file if it's in use
                }
            }
        }
    }
}
