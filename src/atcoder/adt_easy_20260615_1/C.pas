program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	sysutils, strutils;
var
	n, m, i, j, c, w: int32;
	s, t, line: string;
	sa: TStringArray;
	found: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(line);
	sa := SplitString(line, ' '); 

	s := sa[0];
	t := sa[1];

	n := length(s);
	m := length(t);

	found := false;
	for w := n-1 downto 1 do
		for c := 1 to w do
			if not found then begin
				i := c;
				j := 1;
				while (i <= n) and (j <= m) and (s[i] = t[j]) do begin
					inc(i, w);
					inc(j);
				end;
				found := (i > n) and (j > m);
			end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
