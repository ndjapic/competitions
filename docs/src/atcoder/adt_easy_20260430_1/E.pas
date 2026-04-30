program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	m, j: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := 3;
	if t[3] = 'X' then dec(m);

	j := 1;
	for i := 1 to n do begin
		s[i] := chr(ord(s[i]) - ord('a') + ord('A'));
		if (j <= m) and (s[i] = t[j]) then inc(j);
	end;

	if j > m then
		writeln('Yes')
	else
		writeln('No');
end.
