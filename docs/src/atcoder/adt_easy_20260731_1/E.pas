program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	m, j: int32;
	s: array of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	m := 0;
	setlength(s, m);

	for i := 1 to n do begin
		setlength(s, 2*m+1);
		s[m] := i;
		for j := m+1 to 2*m do s[j] := s[j-m-1];
		m := 2*m+1;
	end;

	for j := 0 to m-2 do write(s[j], ' ');
	writeln(s[m-1]);
end.
