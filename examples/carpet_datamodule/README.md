These packages are example of how decorate your TDataModule with coloured panel, 
label and images.
It show us how to use the lazarus IDE form designer to
design non LCL visual widgets, and as bonus you can use it in real application.


Installation:
Install the packages: 
* examples/carpet_datamodule/carpetspack.lpk 
* examples/carpet_datamodule/carpet_graphicalpack.lpk 
* examples/carpet_datamodule/carpetspack_designer.lpk 
and restart the IDE.
Then open the example project examples/carpet_datamodule/demo/project1.lpi.
Open the form of unit2.pas and you will see some non LCL controls in the
designer as various colored rectangles.

Overview:
The package "carpetspack" is pure non-lcl, it doesn't depends on graphics.pas, 
so you can use it on any (GUI & No GUI, LCL & non-LCL) application.
it introduces new visual non-LCL components:
* TDataRoom : similar as TDataModule
* TCarpet : similar as TPanel
* TCarpetLabel : similar as TLabel

But "carpetspack_designer" is depends on LCL in design-time only.
Its job is to provide designer and rendering component in designtime.


"carpet_graphicalpack" package introduce TCarpetImage wich is using TPicture, 
so it require LCLBase package. It is optional to install it,
I do not include carpet_image.pas unit to carpetspack package
to keep carpetspack pure non-lcl.
It introduces components:
* TCarpetImage


More info about non-LCL designer in Lazarus, see: examples/designnonlcl/notlcldesigner.lpk
