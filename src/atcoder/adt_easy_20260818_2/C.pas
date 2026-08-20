program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #grid #search #learn
const
	NN = 50;
var
	n, m, a, b, i, j: int8;
	found: boolean;
	s, t: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do readln(s[a]);
	for i := 1 to m do readln(t[i]);

	found := false;
	a := m-1;
	while (a < n) and not found do begin
		inc(a);
		b := m-1;
		while (b < n) and not found do begin
			inc(b);

			i := 1;
			found := true;
			while (i <= m) and found do begin
				j := 1;
				while (j <= m) and found do begin
					found := s[a-m+i, b-m+j] = t[i, j];
					inc(j);
				end;
				inc(i);
			end;

		end;
	end;

	writeln(a-m+1, ' ', b-m+1);
end.
