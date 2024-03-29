# Revision history for blucontrol

## 0.7.1.0 *16 Jan 2024*

* Support GHC 9.6.

## 0.7.0.0 *19 Jul 2021*

* `MonadGamma` has been renamed to `MonadPrepareValue`.
* `MonadRecolor` has been renamed to `MonadApplyValue`.
* Move modules from `Blucontrol.Monad.Gamma` to `Blucontrol.Monad.PrepareValue`.
* Move modules from `Blucontrol.Monad.Recolor` to `Blucontrol.Monad.ApplyValue`.

## 0.6.0.0 *24 Jun 2021*

* `runControl` is now a regular monad runner, that also returns the monadic state.
* `blucontrol` now returns the monadic state.
* Changes to the control flow in `Blucontrol.Main.Control` improve how monadic state is handled.

## 0.5.1.1 *21 Jun 2021*

* Allow building against new versions of X11.

## 0.5.1.0 *19 Jun 2021*

* `RecolorXT` now uses `RecolorXValue`, which is just a newtype of the old `RGB Float`.
* Add `applyBrightnessToRGB` conversion function.

## 0.5.0.0 *09 Jun 2021*

* Change module structure, now using `Blucontrol.Monad` and `Blucontrol.Value`.
* Move module `Blucontrol.Value.RGB.Brightness` to `Blucontrol.Value.Brightness`.
* Rename module `Blucontrol.CompatibleValues` to `Blucontrol.Value`.

## 0.4.1.0 *08 Jun 2021*

* Remove field `coerceValue` from `ConfigControl`.
* Add `CompatibleValues` class.

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
