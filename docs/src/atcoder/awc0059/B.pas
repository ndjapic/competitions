program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, SysUtils, Math;
const
	nn = 100 * 1000;
	mm = 10;
var
	n, k, i, s: int32;
	m, j: int8;
	t: array [1 .. mm] of int32;
	p: array [1 .. nn] of int32;
	qualified: array [1 .. nn] of boolean;
	candidates: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareStudents(constref Left, Right: int32): int32;
begin
	Result := CompareValue(p[Right], p[Left]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for j := 1 to m do read(t[j]);
	readln;

	candidates := tlist<int32>.create;

	for i := 1 to n do begin
		p[i] := 0;
		qualified[i] := true;
		for j := 1 to m do begin
			read(s);
			qualified[i] := qualified[i] and (s >= t[j]);
			inc(p[i], s);
		end;
		if qualified[i] then candidates.add(i);
	end;
	readln;

	candidates.sort(tcomparer<int32>.construct(CompareStudents));

	while (candidates.count > k) and (p[candidates[k-1]] = p[candidates[k]]) do inc(k);

	for i := k to candidates.count - 1 do qualified[candidates[i]] := false;

	for i := 1 to n do if qualified[i] then writeln(i);

	candidates.free;
end.
