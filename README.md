# OPI - Open Perimetry Interface

The Open Perimetry Interface (OPI) can be used for controlling visual field machines using [R](http://www.r-project.org/). It specifies basic functions that allow many visual field tests to be constructed. It is part of the [Open Perimetry Initiative](http://perimetry.org/opi).

Once the package is downloaded and installed through R, it can be used to control the perimeters listed under "Supported Perimeters", with library routines to perform thresholding using the algorithms listed under the "Test Procedures".

## Supported Perimeters
- [Octopus 900](http://www.haag-streit.com/products/perimetry/octopusr-900.html) / [Haag-Streit](http://www.haag-streit.com/)
- [Octopus 600 (limited implementation)](http://www.haag-streit.com/products/perimetry/octopusr-900.html) / [Haag-Streit](http://www.haag-streit.com/)
- [HEP (Heidelberg Edge Perimeter)](http://www.heidelbergengineering.com/germany/produkte/hep/) / [Heidelberg Engineering](http://www.heidelbergengineering.com)
- [AP700](https://www.kowa.co.jp/e-life/product/perimeter.htm)[Kowa](https://www.kowa.co.jp/eng/)
- [Compass](https://www.icare-world.com/us/product/icare-compass/?gclid=CjwKCAjw3cSSBhBGEiwAVII0Z6jzmIYWoQH5tJ3ewSIoAxTKizmJCsFW_Xv1wooLnIYzBzpl8oP3NRoCtTwQAvD_BwE) / [iCare](https://www.icare-world.com/)
- [imo](https://www.crewt.co.jp/en/products/)[CREWT Medical Systems Inc](https://www.crewt.co.jp/en/)
- [Google Cardboard](https://arvr.google.com/cardboard/) (Also can be used with other android systems: PhoneVR)
- A variety of built in simulators

## Test Procedures
- MoCS - [Method of constant stimuli (Wikipedia)](http://en.wikipedia.org/wiki/Psychophysics#Method_of_constant_stimuli)
- ZEST - [King-Smith et al. 1993](https://www.sciencedirect.com/science/article/pii/0042698994900396)
- Full Threshold - Best guess at the original HFA algorithm based on a 4-2 staircase
- QUEST+ - [General Bayesian framework for multiple dimensions](https://jov.arvojournals.org/article.aspx?articleid=2611972)
