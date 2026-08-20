program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #hashset
uses
	generics.collections, generics.defaults, math;
const
	NN = 1000;
var
	n, i: int32;
	found: boolean;
	s: string;
	d: tdictionary<string, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	d := tdictionary<string, boolean>.create;
	found := false;

	for i := 1 to n do begin
		readln(s);
		if not found then begin
			if d.containskey(s) then
				found := true
			else
				d.addorsetvalue(s, true);
		end;
	end;
	d.free;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
