using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

namespace APSIM.Services.Documentation
{
    /// <summary>A tag which displays an image.</summary>
    public class Image : ITag
    {
        private System.Drawing.Image raster;
        private string resourceName;

        /// <summary>The image to put into the doc.</summary>
        public System.Drawing.Image GetRaster()
        {
            if (raster != null)
                return raster;
            if (string.IsNullOrWhiteSpace(resourceName))
                throw new InvalidOperationException("Unable to load image: resource name not specified");
            using (Stream stream = LoadFromResource(resourceName))
                return System.Drawing.Image.FromStream(stream);
        }

        private static Stream LoadFromResource(string resourceName)
        {
            foreach (Assembly assembly in GetAssemblies())
            {
                Stream stream = assembly.GetManifestResourceStream(resourceName);
                if (stream != null)
                    return stream;
                string fullName = assembly.GetManifestResourceNames().FirstOrDefault(n => n.Contains(resourceName));
                if (fullName != null)
                    return assembly.GetManifestResourceStream(fullName);
            }
            throw new FileNotFoundException($"Unable to load image from resource name '{resourceName}': resource not found");
        }

        private static IEnumerable<Assembly> GetAssemblies()
        {
            return new string[]
            {
                "APSIM.Interop",
                "ApsimNG",
                "Models",
                "APSIM.Services",
            }.Select(GetAssembly)
             .Where(a => a != null);
        }

        private static Assembly GetAssembly(string name)
        {
            return AppDomain.CurrentDomain.GetAssemblies().FirstOrDefault(a => a.GetName().Name == name);
        }

        /// <summary>
        /// Create an Image tag for a given image object.
        /// </summary>
        /// <param name="image">The image.</param>
        public Image(System.Drawing.Image image) => raster = image;

        /// <summary>
        /// Create an Image tag from a resource name. The resource name
        /// can be just the file name (e.g. "AIBanner.png") or can be the
        /// full path including the assembly name (e.g.
        /// "ApsimNG.Resources.AIBanner.png).
        /// </summary>
        /// <param name="resource">Name of the resource.</param>
        public Image(string resource)
        {
            resourceName = resource;
        }
    }
}
