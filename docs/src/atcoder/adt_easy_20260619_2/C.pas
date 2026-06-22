program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bowling #split
var
	s: string;
	l, r, i: int8;
	col: array [-3 .. 3] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	col[-3] := s[7] = '1';
	col[-2] := s[4] = '1';
	col[-1] := (s[2] = '1') or (s[8] = '1');
	col[0] := (s[1] = '1') or (s[5] = '1');
	col[1] := (s[3] = '1') or (s[9] = '1');
	col[2] := s[6] = '1';
	col[3] := s[10] = '1';

	l := -3;
	r := 3;
	while (l < r) and not col[l] do inc(l);
	while (l < r) and not col[r] do dec(r);

	i := l+1;
	while (i < r) and col[i] do inc(i);

	if (i < r) and (s[1] = '0') then
		writeln('Yes')
	else
		writeln('No');
end.
