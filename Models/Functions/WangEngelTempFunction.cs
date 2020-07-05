﻿using System;
using System.Collections.Generic;
using Models.Core;
using Models.Interfaces;

namespace Models.Functions
{
    /// <summary>
    /// # [Name]
    /// A function that adds values from child functions
    /// </summary>
    [Serializable]
    [Description("Calculates relative temperature response")]
    [ViewName("UserInterface.Views.GridView")]
    [PresenterName("UserInterface.Presenters.PropertyPresenter")]

    public class 
        WangEngelTempFunction: Model, IFunction, ICustomDocumentation, IIndexedFunction
        {
        
        /// <summary>Minimum Temperature.</summary>
        [Description("Minimum Temperature")]
        public double MinTemp { get; set; }
        /// <summary>Optimum Temperature</summary>
        [Description("Optimum Temperature")]
        public double OptTemp { get; set; }
        /// <summary>Maximum Temperature</summary>
        [Description("Maximum Temperature")]
        public double MaxTemp { get; set; }
        /// <summary>The met data</summary>

        [Description("Reference Temperature (MinTemp<RefTemp<MaxTemp)")]
        public double RefTemp { get; set; }
        /// <summary>The met data</summary>

        [Link]
        protected IWeather MetData = null;
        
        /// <summary>The maximum temperature weighting</summary>
        [Description("Maximum Temperature Weighting")]
        public double MaximumTemperatureWeighting { get; set; }

        /// <summary>Gets the value.</summary>
        public double Value(int arrayIndex = -1)
        {
            double Tav = MaximumTemperatureWeighting * MetData.MaxT + (1 - MaximumTemperatureWeighting) * MetData.MinT;
            return ValueIndexed(Tav);
        }


        /// <summary>
        /// returns result of Wang Eagle beta function for given temperature
        /// </summary>
        /// <param name="T">Given temperature</param>
        /// <returns></returns>
        public double ValueIndexed(double T)
        {
            double RelEff = 0.0;
            double RelEffRefTemp = 1.0;
            double p = 0.0;
            
            if ((T > MinTemp) && (T < MaxTemp))
            {
                p = Math.Log(2.0) / Math.Log((MaxTemp - MinTemp) / (OptTemp - MinTemp));
                RelEff = (2 * Math.Pow(T - MinTemp, p) * Math.Pow(OptTemp - MinTemp, p) - Math.Pow(T - MinTemp, 2 * p)) / Math.Pow(OptTemp - MinTemp, 2 * p);
            }

            if ((RefTemp > MinTemp) && (RefTemp < MaxTemp))
            {
                p = Math.Log(2.0) / Math.Log((MaxTemp - MinTemp) / (OptTemp - MinTemp));
                RelEffRefTemp = (2 * Math.Pow(RefTemp - MinTemp, p) * Math.Pow(OptTemp - MinTemp, p) - Math.Pow(RefTemp - MinTemp, 2 * p)) / Math.Pow(OptTemp - MinTemp, 2 * p);
            }

            return RelEff / RelEffRefTemp;
        }

        /// <summary>Writes documentation for this function by adding to the list of documentation tags.</summary>
        /// <param name="tags">The list of tags to add to.</param>
        /// <param name="headingLevel">The level (e.g. H2) of the headings.</param>
        /// <param name="indent">The level of indentation 1, 2, 3 etc.</param>
        public void Document(List<AutoDocumentation.ITag> tags, int headingLevel, int indent)
        {
            if (IncludeInDocumentation)
                SubtractFunction.DocumentMathFunction(this, '+', tags, headingLevel, indent);
        }
    }

}