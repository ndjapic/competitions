program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	ans: int64;
	ch: char;
	s: string;
	same: boolean;
	c: array ['a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for ch := 'a' to 'z' do c[ch] := 0;

	ans := 0;
	same := false;
	for i := 1 to n do begin
		inc(c[s[i]]);
		inc(ans, i - c[s[i]]);
		if c[s[i]] > 1 then same := true;
	end;

	if same then inc(ans);
	writeln(ans);
end.
