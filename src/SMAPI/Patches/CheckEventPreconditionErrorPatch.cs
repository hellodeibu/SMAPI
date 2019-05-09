using System.Diagnostics.CodeAnalysis;
using System.Reflection;
using Harmony;
using StardewModdingAPI.Framework.Patching;
using StardewValley;

namespace StardewModdingAPI.Patches
{
    /// <summary>A Harmony patch for the <see cref="Dialogue"/> constructor which intercepts invalid dialogue lines and logs an error instead of crashing.</summary>
    internal class CheckEventPreconditionErrorPatch : IHarmonyPatch
    {
        /*********
        ** Fields
        *********/
        /// <summary>Writes messages to the console and log file on behalf of the game.</summary>
        private static IMonitor MonitorForGame;

        /// <summary>Whether the method is currently being intercepted.</summary>
        private static bool IsIntercepted;


        /*********
        ** Accessors
        *********/
        /// <summary>A unique name for this patch.</summary>
        public string Name => $"{nameof(CheckEventPreconditionErrorPatch)}";


        /*********
        ** Public methods
        *********/
        /// <summary>Construct an instance.</summary>
        /// <param name="monitorForGame">Writes messages to the console and log file on behalf of the game.</param>
        public CheckEventPreconditionErrorPatch(IMonitor monitorForGame)
        {
            CheckEventPreconditionErrorPatch.MonitorForGame = monitorForGame;
        }

        /// <summary>Apply the Harmony patch.</summary>
        /// <param name="harmony">The Harmony instance.</param>
        public void Apply(HarmonyInstance harmony)
        {
            harmony.Patch(
                original: AccessTools.Method(typeof(GameLocation), "checkEventPrecondition"),
                prefix: new HarmonyMethod(this.GetType(), nameof(CheckEventPreconditionErrorPatch.Prefix))
            );
        }


        /*********
        ** Private methods
        *********/
        /// <summary>The method to call instead of the GameLocation.CheckEventPrecondition.</summary>
        /// <param name="__instance">The instance being patched.</param>
        /// <param name="__result">The return value of the original method.</param>
        /// <param name="precondition">The precondition to be parsed.</param>
        /// <param name="__originalMethod">The method being wrapped.</param>
        /// <returns>Returns whether to execute the original method.</returns>
        /// <remarks>This method must be static for Harmony to work correctly. See the Harmony documentation before renaming arguments.</remarks>
        [SuppressMessage("ReSharper", "InconsistentNaming", Justification = "Argument names are defined by Harmony.")]
        private static bool Prefix(GameLocation __instance, ref int __result, string precondition, MethodInfo __originalMethod)
        {
            if (CheckEventPreconditionErrorPatch.IsIntercepted)
                return true;

            try
            {
                CheckEventPreconditionErrorPatch.IsIntercepted = true;
                __result = (int)__originalMethod.Invoke(__instance, new object[] { precondition });
                return false;
            }
            catch (TargetInvocationException ex)
            {
                __result = -1;
                CheckEventPreconditionErrorPatch.MonitorForGame.Log($"Failed parsing event precondition ({precondition}):\n{ex.InnerException}", LogLevel.Error);
                return false;
            }
            finally
            {
                CheckEventPreconditionErrorPatch.IsIntercepted = false;
            }
        }
    }
}
