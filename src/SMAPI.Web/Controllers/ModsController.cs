using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Options;
using StardewModdingAPI.Toolkit;
using StardewModdingAPI.Web.Framework.Caching.Wiki;
using StardewModdingAPI.Web.Framework.ConfigModels;
using StardewModdingAPI.Web.ViewModels;

namespace StardewModdingAPI.Web.Controllers
{
    /// <summary>Provides user-friendly info about SMAPI mods.</summary>
    internal class ModsController : Controller
    {
        /*********
        ** Fields
        *********/
        /// <summary>The cache in which to store mod metadata.</summary>
        private readonly IWikiCacheRepository Cache;

        /// <summary>The number of minutes successful update checks should be cached before refetching them.</summary>
        private readonly int CacheMinutes;


        /*********
        ** Public methods
        *********/
        /// <summary>Construct an instance.</summary>
        /// <param name="cache">The cache in which to store mod metadata.</param>
        /// <param name="configProvider">The config settings for mod update checks.</param>
        public ModsController(IWikiCacheRepository cache, IOptions<ModCompatibilityListConfig> configProvider)
        {
            ModCompatibilityListConfig config = configProvider.Value;

            this.Cache = cache;
            this.CacheMinutes = config.CacheMinutes;
        }

        /// <summary>Display information for all mods.</summary>
        [HttpGet]
        [Route("mods")]
        public async Task<ViewResult> Index()
        {
            return this.View("Index", await this.FetchDataAsync());
        }


        /*********
        ** Private methods
        *********/
        /// <summary>Asynchronously fetch mod metadata from the wiki.</summary>
        public async Task<ModListModel> FetchDataAsync()
        {
            // refresh cache
            CachedWikiMod[] mods;
            if (!this.Cache.TryGetWikiMetadata(out CachedWikiMetadata metadata) || this.Cache.IsStale(metadata.LastUpdated, this.CacheMinutes))
            {
                var wikiCompatList = await new ModToolkit().GetWikiCompatibilityListAsync();
                this.Cache.SaveWikiData(wikiCompatList.StableVersion, wikiCompatList.BetaVersion, wikiCompatList.Mods, out metadata, out mods);
            }
            else
                mods = this.Cache.GetWikiMods().ToArray();

            // build model
            return new ModListModel(
                stableVersion: metadata.StableVersion,
                betaVersion: metadata.BetaVersion,
                mods: mods
                    .Select(mod => new ModModel(mod.GetModel()))
                    .OrderBy(p => Regex.Replace(p.Name.ToLower(), "[^a-z0-9]", "")) // ignore case, spaces, and special characters when sorting
            );
        }
    }
}
