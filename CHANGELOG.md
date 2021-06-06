# Revision history for blucontrol

## 0.4.0.0 *06 Jun 2021*

* Remove `RGB` class.
* Remove `Chromaticity`.
* Replace `Trichromaticity` by `RGB`.
* Add type families `GammaValue` and `RecolorValue` to `MonadGamma` and `MonadRecolor`.
* Add `coerceValue` to `ConfigControl`.
* `RecolorXT` now uses `RGB Float` instead of `Trichromaticity`.
* `Temperature` is now explicitly convertible to `RGB Word8` using `toRGBWord8`.
* Rename parametric field of `WithBrightness` record from `rgb` to `color`.

## 0.3.2.0 *04 Jun 2021*

* New module `Blucontrol.RGB.Brightness`.
* Introduce strictness to improve performance.
* Allow `runGamma` to directly run in the `IO` monad instead of `g` with `MonadGamma c g`.

## 0.3.1.0 *30 May 2021*

* New module `Blucontrol.Gamma.Modifier`.
* New command line flag `-i`/`--ignore-config` to stick to the default configuration.

## 0.3.0.1 *28 Apr 2021*

* Support GHC 9.0.1.

## 0.3.0.0 *18 Apr 2021*

* Add `RGB` class.
* Remove `temperature` conversion function in favor of `toRGB`.
* `GammaLinearT` now interpolates between `Temperature`s instead of `Trichromaticity`s.

## 0.2.1.1 *12 Aug 2020*

* Improve miscallenous documentation.
* Add `Setup.hs`.

## 0.2.1.0 *12 Aug 2020*

* Compile user config only when it has been modified.
* New internal module `Blucontrol.Main.GHC.Internal` for setting ghc flags.

## 0.2.0.0 *10 Aug 2020*

* Rename the whole application from bludigon to blucontrol.

## 0.1.1.0 *10 Aug 2020*

* `runGamma` runs directly in `IO` now.
* New module `Bludigon.Control.Concat`.
* New module `Bludigon.Control.Count`.

## 0.1.0.1 *02 Aug 2020*

* Add header file to c-sources.

## 0.1.0.0 *02 Aug 2020*

* First version. Released on an unsuspecting world.
