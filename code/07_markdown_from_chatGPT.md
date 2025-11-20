

# 🐾 **Markdown Explanation of the Camera Trap Activity Analysis Code**

This code analyzes daily activity patterns of wildlife species using kernel density estimation and overlap analyses. It uses the **overlap** package, which is commonly applied in ecological studies involving camera-trap data.

---

## 📦 **Load the Required Package**

```r
#install.packages("overlap")
library(overlap)
```

The **overlap** package provides functions such as `densityPlot()` and `overlapPlot()` for estimating and visualizing activity patterns of animals based on circular time data.

---

## 📁 **Load the Sample Dataset**

```r
data(kerinci)
```

The `kerinci` dataset is included in the package.
It contains camera-trap records from Kerinci Seblat National Park (Sumatra), including:

* species (`Sps`)
* time of detection (`time`, expressed in linear scale 0–1)

---

## 🔄 **Convert Linear Time to Circular Time**

```r
circulartime <- kerinci$time * 2 * pi
kerinci$Timecirc <- kerinci$time * 2 * pi
```

Because daily time is **circular** (after 24h it loops back to 0), we convert the linear time variable (0–1 scale) into circular radians (0–2π).
This is required for kernel activity estimation.

---

## 🐅 **Extract Data for Tigers**

```r
tiger <- kerinci[kerinci$Sps == "tiger", ]
```

This filters the dataset so we only keep rows where the species name is *tiger*.

---

## 📈 **Plot the Kernel Density of Tiger Activity**

```r
densityPlot(tiger$Timecirc)
tigertime <- tiger$Timecirc
densityPlot(tigertime)
```

`densityPlot()` estimates how active the species is across the 24-hour cycle.

Tigers typically show:

* a morning activity peak
* another peak around dusk or night

This reflects their crepuscular/nocturnal behavior.

---

## 🐒 **Exercise: Plot Activity for Macaques**

```r
macaque <- kerinci[kerinci$Sps == "macaque", ]
macaquetime <- macaque$Timecirc
densityPlot(macaque$Timecirc)
```

Macaques usually show:

* a strong activity peak in the morning
* reduced activity at night

This reflects their **diurnal** (day-active) lifestyle.

---

## ❓ **Why Are Tiger and Macaque Peaks Different?**

* **Tigers**: crepuscular/nocturnal predators → active at dawn, dusk, and night
* **Macaques**: diurnal primates → active mainly in daylight hours

Thus, their activity patterns naturally differ due to their ecological niches and behavior.

---

## 🔁 **Compare Overlap Between the Two Species**

```r
overlapPlot(tigertime, macaquetime)
```

This visualizes how much the two activity curves overlap.
Interpretation:

* A high overlap suggests similar activity times
* A low overlap indicates temporal separation (i.e., avoiding each other)

In this case, tiger and macaque likely show *moderate to low overlap*, because one is mainly nocturnal and the other diurnal.

---

