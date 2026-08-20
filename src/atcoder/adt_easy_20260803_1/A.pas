program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ch: char;
	seen: array ['a' .. 'z'] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for ch := 'a' to 'z' do seen[ch] := false;
	for ch in s do seen[ch] := true;

	ch := 'a';
	while seen[ch] do inc(ch);
	writeln(ch);
end.
