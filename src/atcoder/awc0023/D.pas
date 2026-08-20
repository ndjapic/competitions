program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults, math;
const
	nn = 50;
	ww = 100 * 100;
var
	n, i: int8;
	s, t, ans: int32;
	p, c, w, op, ow: array [1 .. nn] of int32;
	sp, sw: array [0 .. nn] of int32;
	ind: tlist<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(i, d: int8; x, y: int32);
begin
	if (x > s) or (ans <= d) or (y + sp[i] < t) then
	else if y >= t then
		ans := d
	else if (i > 0) and (op[i] > 0) then begin
		dfs(i-1, d+1, x + ow[i], y + op[i]);
		dfs(i-1, d, x, y);
	end;
end;

function CompareIndices(constref left, right: int8): int32;
begin
	result := p[left] - p[right];
	if result = 0 then
		result := w[left] - w[right];
	{result := sign(p[left] - p[right] + w[right] - w[left]);}
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, t);

	ind := tlist<int8>.create;
	for i := 1 to n do begin
		readln(p[i], c[i], w[i]);
		dec(p[i], c[i]);
		ind.add(i);
	end;
	ind.sort(tcomparer<int8>.construct(CompareIndices));

	sw[0] := 0;
	sp[0] := 0;

	for i := 1 to n do begin
		ow[i] := w[ind[i-1]];
		op[i] := p[ind[i-1]];
		sw[i] := sw[i-1] + ow[i];
		sp[i] := sp[i-1] + max(0, op[i]);
	end;

	ans := n+1;
	dfs(n, 0, 0, 0);

	if ans > n then ans := -1;
	writeln(ans);
	ind.free;
end.
