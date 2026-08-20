program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #geometry #trigonometry #transformation #rotation #matrix #multiplication #polar #coordinate #change
var
	a, b, d, co, si: real;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, d);

	d := d / 180 * pi;
	co := cos(d);
	si := sin(d);

	writeln(a*co - b*si :0:6, ' ', a*si + b*co :0:6);
end.
