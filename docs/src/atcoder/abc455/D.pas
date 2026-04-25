program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 300 * 1000;
var
	n, q, i, j, c, p, ans: int32;
	prev, next: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do begin
		prev[i] := 0;
		next[i] := 0;
	end;

	for j := 1 to q do begin
		readln(c, p);
		if prev[c] > 0 then next[prev[c]] := 0;
		next[p] := c;
		prev[c] := p;
	end;

	for i := 1 to n do begin
		ans := 0;
		if prev[i] = 0 then begin
			c := i;
			while c > 0 do begin
				inc(ans);
				c := next[c];
			end;
		end;
		write(ans);
		if i < n then write(' ');
	end;
	writeln;
end.
