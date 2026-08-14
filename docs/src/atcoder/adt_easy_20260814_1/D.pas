program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #hashmap #counter
uses
	generics.collections;
const
	NN = 1000;
var
	n, m, i, j, a, b, c: int32;
	ans: boolean;
	d: tdictionary<int32, int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	d := tdictionary<int32, int32>.create;
	for i := 1 to n do begin
		read(a);
		if not d.trygetvalue(a, c) then c := 0;
		d.addorsetvalue(a, c+1);
	end;
	readln;

	ans := true;
	for j := 1 to m do begin
		read(b);
		if ans then begin
			if not d.trygetvalue(b, c) or (c = 0) then
				ans := false
			else
				d.addorsetvalue(b, c-1);
		end;
	end;
	readln;

	if ans then
		writeln('Yes')
	else
		writeln('No');
	d.free;
end.
