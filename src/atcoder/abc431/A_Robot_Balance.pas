program A_Robot_Balance;
uses
	math;
var
	h, b: int8;

begin
	readln(h, b);
	writeln(max(0, h-b));
end.
