program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults, math;
var
	n, k, i, elm, l, r, ans: int32;
	x: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	x := tlist<int32>.create;
	for i := 0 to n-1 do begin
		read(elm);
		x.add(elm);
		x.exchange(i, random(i+1));
	end;
	readln;
	x.sort;

	ans := 0;
	l := 0;
	for r := 0 to n-1 do begin
		while x[r] - x[l] > k do inc(l);
		ans := max(ans, r-l+1);
	end;

	writeln(ans);
	x.free;
end.
