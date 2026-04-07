program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults, math;
const
	nn = 200 * 1000;
var
	n, i, d, p, fi: int32;
	ans, s: int64;
	f: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, d, p);
	f := tlist<int32>.create;

	for i := 0 to n-1 do begin
		read(fi);
		f.add(fi);
		f.exchange(i, random(i+1));
	end;
	f.sort;

	s := 0;
	ans := int64(n+d-1) div d * p;
	for i := 1 to n do begin
		inc(s, f[i-1]);
		ans := min(ans, s + int64(n-i+d-1) div d * p);
	end;

	writeln(ans);
	f.free;
end.
