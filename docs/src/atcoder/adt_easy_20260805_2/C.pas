program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	ch: char;
	s, t: string;
	seen: array ['A' .. 'z'] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);

	for ch := 'A' to 'z' do seen[ch] := false;

	for ch in t do seen[ch] := true;

	i := 2;
	while (i <= n) and ((s[i] > 'Z') or seen[s[i-1]]) do inc(i);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.
