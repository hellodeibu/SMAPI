using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using StardewModdingAPI.Toolkit.Framework.Clients.Wiki;

namespace StardewModdingAPI.Web.Framework.Caching.Wiki
{
    /// <summary>Encapsulates logic for accessing the mod data cache.</summary>
    internal interface IWikiCacheRepository
    {
        /*********
        ** Methods
        *********/
        /// <summary>Get the cached wiki metadata.</summary>
        /// <param name="metadata">The fetched metadata.</param>
        bool TryGetWikiMetadata(out CachedWikiMetadata metadata);

        /// <summary>Whether cached data is stale.</summary>
        /// <param name="lastUpdated">The date when the data was updated.</param>
        /// <param name="cacheMinutes">The age in minutes before data is considered stale.</param>
        bool IsStale(DateTimeOffset lastUpdated, int cacheMinutes);

        /// <summary>Get the cached wiki mods.</summary>
        /// <param name="filter">A filter to apply, if any.</param>
        IEnumerable<CachedWikiMod> GetWikiMods(Expression<Func<CachedWikiMod, bool>> filter = null);

        /// <summary>Save data fetched from the wiki compatibility list.</summary>
        /// <param name="stableVersion">The current stable Stardew Valley version.</param>
        /// <param name="betaVersion">The current beta Stardew Valley version.</param>
        /// <param name="mods">The mod data.</param>
        /// <param name="cachedMetadata">The stored metadata record.</param>
        /// <param name="cachedMods">The stored mod records.</param>
        void SaveWikiData(string stableVersion, string betaVersion, IEnumerable<WikiModEntry> mods, out CachedWikiMetadata cachedMetadata, out CachedWikiMod[] cachedMods);
    }
}
