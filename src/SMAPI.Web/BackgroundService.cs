using System;
using System.Threading;
using System.Threading.Tasks;
using Hangfire;
using Microsoft.Extensions.Hosting;
using StardewModdingAPI.Toolkit;
using StardewModdingAPI.Toolkit.Framework.Clients.Wiki;
using StardewModdingAPI.Web.Framework.Caching.Wiki;

namespace StardewModdingAPI.Web
{
    /// <summary>A hosted service which runs background data updates.</summary>
    /// <remarks>Task methods need to be static, since otherwise Hangfire will try to serialise the entire instance.</remarks>
    internal class BackgroundService : IHostedService, IDisposable
    {
        /*********
        ** Fields
        *********/
        /// <summary>The background task server.</summary>
        private static BackgroundJobServer JobServer;

        /// <summary>The cache in which to store mod metadata.</summary>
        private static IWikiCacheRepository WikiCache;


        /*********
        ** Public methods
        *********/
        /****
        ** Hosted service
        ****/
        /// <summary>Construct an instance.</summary>
        /// <param name="wikiCache">The cache in which to store mod metadata.</param>
        public BackgroundService(IWikiCacheRepository wikiCache)
        {
            BackgroundService.WikiCache = wikiCache;
        }

        /// <summary>Start the service.</summary>
        /// <param name="cancellationToken">Tracks whether the start process has been aborted.</param>
        public Task StartAsync(CancellationToken cancellationToken)
        {
            this.TryInit();

            // set startup tasks
            BackgroundJob.Enqueue(() => BackgroundService.UpdateWikiAsync());

            // set recurring tasks
            RecurringJob.AddOrUpdate(() => BackgroundService.UpdateWikiAsync(), "*/10 * * * *");

            return Task.CompletedTask;
        }

        /// <summary>Triggered when the application host is performing a graceful shutdown.</summary>
        /// <param name="cancellationToken">Tracks whether the shutdown process should no longer be graceful.</param>
        public async Task StopAsync(CancellationToken cancellationToken)
        {
            if (BackgroundService.JobServer != null)
                await BackgroundService.JobServer.WaitForShutdownAsync(cancellationToken);
        }

        /// <summary>Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.</summary>
        public void Dispose()
        {
            BackgroundService.JobServer?.Dispose();
        }

        /****
        ** Tasks
        ****/
        /// <summary>Update the cached wiki metadata.</summary>
        public static async Task UpdateWikiAsync()
        {
            WikiModList wikiCompatList = await new ModToolkit().GetWikiCompatibilityListAsync();
            BackgroundService.WikiCache.SaveWikiData(wikiCompatList.StableVersion, wikiCompatList.BetaVersion, wikiCompatList.Mods, out _, out _);
        }


        /*********
        ** Private method
        *********/
        /// <summary>Initialise the background service if it's not already initialised.</summary>
        /// <exception cref="InvalidOperationException">The background service is already initialised.</exception>
        private void TryInit()
        {
            if (BackgroundService.JobServer != null)
                throw new InvalidOperationException("The scheduler service is already started.");

            BackgroundService.JobServer = new BackgroundJobServer();
        }
    }
}
