﻿using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace StardewModdingAPI.Helpers
{
    public static class CecilHelper
    {
        //System.Void StardewValley.Game1::.ctor()
        
        private static void InjectMethod(ILProcessor ilProcessor, Instruction target, MethodReference method)
        {
            var callTarget = target;
            if (method.HasParameters)
            {
                Instruction loadObjInstruction = ilProcessor.Create(OpCodes.Ldarg_0);
                ilProcessor.InsertBefore(target, loadObjInstruction);
                callTarget = loadObjInstruction;
            }
            Instruction callEnterInstruction = ilProcessor.Create(OpCodes.Call, method);
            ilProcessor.InsertAfter(callTarget, callEnterInstruction);
        }

        private static void InjectMethod(ILProcessor ilProcessor, IEnumerable<Instruction> targets, MethodReference method)
        {
            foreach(var target in targets.ToList())
            {
                InjectMethod(ilProcessor, target, method);
            }
        }

        private static List<Instruction> GetMatchingInstructions(Collection<Instruction> instructions, OpCode opcode, object @object)
        {
            return instructions.Where(n => n.OpCode == opcode && n.Operand == @object).ToList();            
        }

        public static void RedirectConstructor(CecilContext stardewContext, CecilContext smapiContext,
            string typeToAlter, string methodToAlter,
            string injecteeType, string injecteeMethod,
            string injectedType, string injectedMethod)
        {
            var ilProcessor = stardewContext.GetMethodILProcessor(typeToAlter, methodToAlter);
            var methodDefinition = stardewContext.GetMethodDefinition(injecteeType, injecteeMethod);

            var methodInfo = smapiContext.GetSMAPITypeContructor(injectedType);
            var reference = smapiContext.ImportSMAPIMethodInStardew(stardewContext, methodInfo);

            var instructionsToAlter = GetMatchingInstructions(ilProcessor.Body.Instructions, OpCodes.Newobj, methodDefinition);

            var newInstruction = ilProcessor.Create(OpCodes.Newobj, reference);
            foreach(var instruction in instructionsToAlter)
            {
                ilProcessor.Replace(instruction, newInstruction);
            }
        }

       // public void ReplaceInstruction(ILProcessor processor, OpCode opcode, string oldOperand, string newOperand)
        //{
            //var instructions = processor.Body.Instructions.Where(i => i.OpCode == opcode && i.Operand == oldOperand);
           // processor.Create()
        //}

        public static void InjectEntryMethod(CecilContext stardewContext, CecilContext smapiContext, string injecteeType, string injecteeMethod, 
            string injectedType, string injectedMethod)
        {
            var methodInfo = smapiContext.GetSMAPIMethodReference(injectedType, injectedMethod);
            var reference = smapiContext.ImportSMAPIMethodInStardew(stardewContext, methodInfo);
            var ilProcessor = stardewContext.GetMethodILProcessor(injecteeType, injecteeMethod);
            InjectMethod(ilProcessor, ilProcessor.Body.Instructions.First(), reference);
        }

        public static void InjectExitMethod(CecilContext stardewContext, CecilContext smapiContext, string injecteeType, string injecteeMethod,
            string injectedType, string injectedMethod)
        {
            var methodInfo = smapiContext.GetSMAPIMethodReference(injectedType, injectedMethod);
            var reference = smapiContext.ImportSMAPIMethodInStardew(stardewContext, methodInfo);
            var ilProcessor = stardewContext.GetMethodILProcessor(injecteeType, injecteeMethod);           
            InjectMethod(ilProcessor, ilProcessor.Body.Instructions.Where(i => i.OpCode == OpCodes.Ret), reference);
        }
    }
}
