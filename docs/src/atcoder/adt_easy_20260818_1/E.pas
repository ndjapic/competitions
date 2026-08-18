program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	generics.collections, generics.defaults, math;
const
	NN = 300 * 1000;
var
	n, w, i, mn: int32;
	ans: int64;
	a, b: array [1 .. NN] of int32;
	p: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function cmp(constref l, r: int32): int32;
begin
	result := comparevalue(a[r], a[l]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, w);

	p := tlist<int32>.create;
	for i := 1 to n do begin
		readln(a[i], b[i]);
		p.add(i);
	end;
	p.sort(tcomparer<int32>.construct(cmp));

	ans := 0;
	for i in p do
		if w > 0 then begin
			mn := min(w, b[i]);
			dec(w, mn);
			inc(ans, int64(a[i]) * mn);
		end;

	writeln(ans);
	p.free;
end.
