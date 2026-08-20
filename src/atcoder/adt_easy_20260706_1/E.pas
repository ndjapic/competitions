program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #fix #point #cycle #dag #graph
const
	NN = 200 * 1000;
var
	n, m, i, j, k: int32;
	a, b: array [1 .. NN] of int32;
	seen: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		read(a[i]);
		seen[i] := false;
	end;
	readln;

	i := 1;
	while not seen[i] do begin
		seen[i] := true;
		i := a[i];
	end;

	j := a[i];
	m := 1;
	b[m] := i;
	while j <> i do begin
		inc(m);
		b[m] := j;
		j := a[j];
	end;

	writeln(m);
	for k := 1 to m-1 do write(b[k], ' ');
	writeln(b[m]);
end.
