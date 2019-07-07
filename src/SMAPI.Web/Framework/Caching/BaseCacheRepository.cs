using System;

namespace StardewModdingAPI.Web.Framework.Caching
{
    /// <summary>The base logic for a cache repository.</summary>
    internal abstract class BaseCacheRepository
    {
        /*********
        ** Public methods
        *********/
        /// <summary>Whether cached data is stale.</summary>
        /// <param name="lastUpdated">The date when the data was updated.</param>
        /// <param name="cacheMinutes">The age in minutes before data is considered stale.</param>
        public bool IsStale(DateTimeOffset lastUpdated, int cacheMinutes)
        {
            return lastUpdated < DateTimeOffset.UtcNow.AddMinutes(-cacheMinutes);
        }
    }
}
