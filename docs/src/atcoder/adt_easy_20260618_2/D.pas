program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100;
var
	n, m, i, j: int8;
	s, t: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	m := 0;

	for i := 1 to n do begin
		readln(s[i]);
		m := max(m, length(s[i]));
	end;

	for j := 1 to m do begin
		setlength(t[j], n);
		for i := 1 to n do
			if j <= length(s[i]) then
				t[j][n-i+1] := s[i][j]
			else
				t[j][n-i+1] := '*';
	end;

	i := n;
	for j := 1 to m do begin
		while t[j][i] = '*' do dec(i);
		if i < n then setlength(t[j], i);
		writeln(t[j]);
	end;
end.
