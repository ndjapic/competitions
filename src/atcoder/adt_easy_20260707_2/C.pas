program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000;
var
	n, m, a, c, i: int32;
	seen: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do seen[a] := false;

	for i := 1 to m do begin
		read(a);
		seen[a] := true;
	end;
	readln;

	c := n - m;
	writeln(c);
	a := 0;
	for i := 1 to c do begin
		inc(a);
		while seen[a] do inc(a);
		write(a);
		if i < c then write(' ');
	end;
	writeln;
end.
