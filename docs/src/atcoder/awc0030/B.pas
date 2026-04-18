program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
	inf = 1 shl 30;
var
	n, m, i, j, k, kk: int32;
	p: array [1 .. nn] of int32;
	t: array [1 .. nn] of int8;
	mn: array [0 .. 1] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do readln(p[i], t[i]);

	for j := 1 to m do begin
		read(kk);
		mn[0] := inf;
		mn[1] := inf;

		for k := 1 to kk do begin
			read(i);
			mn[t[i]] := min(mn[t[i]], p[i]);
		end;
		readln;

		if (mn[0] = inf) or (mn[1] = inf) then
			writeln(-1)
		else
			writeln(mn[0] + mn[1]);
	end;
end.
