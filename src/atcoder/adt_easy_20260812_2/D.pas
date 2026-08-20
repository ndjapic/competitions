program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #stack
const
	NN = 100;
var
	n, m, i, j, k, a: int32;
	space: boolean;
	b: array [1 .. NN] of int8;
	seen: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to n do seen[j] := false;

	for i := 1 to m do begin
		read(a);
		seen[a] := true;
	end;
	readln;

	k := 0;
	space := false;
	for j := 1 to n do begin
		inc(k);
		b[k] := j;

		if not seen[j] then
			while k > 0 do begin
				if space then write(' ');
				space := true;
				write(b[k]);
				dec(k);
			end;
	end;
	writeln;
end.
