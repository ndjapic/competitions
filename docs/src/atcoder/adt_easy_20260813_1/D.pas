program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i: int8;
	ch: char;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	if s[1] = s[2] then
		ch := s[1]
	else if s[2] = s[3] then
		ch := s[2]
	else if s[3] = s[1] then
		ch := s[3];

	i := 1;
	while s[i] = ch do inc(i);
	writeln(s[i]);
end.
