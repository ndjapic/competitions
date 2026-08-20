program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort
uses
	generics.collections,
	generics.defaults;
var
	n, i, k, elm: int32;
	x: int64;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k, x);

	a := tlist<int32>.create;
	for i := 0 to n-1 do begin
		read(elm);
		a.add(elm);
		a.exchange(i, random(i+1));
	end;
	readln;
	a.sort;

	i := k;
	while (i > 0) and (x > 0) do begin
		dec(i);
		dec(x, a[i]);
	end;
	a.free;

	if x <= 0 then
		writeln(n-i)
	else
		writeln(-1);
end.
