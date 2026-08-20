program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort
uses
	generics.collections,
	generics.defaults, math;
var
	n, m, i, j: int32;
	k, p, x: int64;
	h: tlist<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, m, k);

	h := tlist<int64>.create;
	for i := 0 to n-1 do begin
		read(x);
		h.add(x);
		h.exchange(i, random(i+1));
	end;
	readln;
	h.sort;

	p := 0;
	for j := 1 to m do begin
		read(x);
		p := max(p, x);
	end;
	readln;

	i := 0;
	while (i < n) and (k >= 0) do begin
		dec(k, (h[i] - 1) div p + 1);
		inc(i);
	end;

	if k < 0 then dec(i);
	writeln(i);
	h.free;
end.
