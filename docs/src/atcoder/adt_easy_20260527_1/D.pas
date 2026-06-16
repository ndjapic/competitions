program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i: int8;
	s, t: string;
	ch: char;
	seen: array ['A' .. 'z'] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);

	for ch := 'A' to 'z' do
		seen[ch] := false;

	for ch in t do
		seen[ch] := true;

	i := length(s);
	while (i > 1) and ((s[i] > 'Z') or seen[s[i-1]]) do dec(i);

	if i > 1 then
		writeln('No')
	else
		writeln('Yes');
end.
