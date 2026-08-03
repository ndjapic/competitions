program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #joke
uses
	SysUtils;
const
	NN = 5000 * 1000;
	PRIME = 1000 * 1000 * 1000 + 7;
var
	k, r: int8;
	ans: int64;
	s, Line: string;
	ch: char;
	Tokens: TStringArray;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(Line);
	Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
	s := Tokens[0];
	k := StrToInt(Tokens[1]);
	r := StrToInt(Tokens[2]);

	ans := 0;

	if k = 1 then begin
		for ch in s do
			ans := (ans * 10 + ord(ch) - ord('0')) mod PRIME;
		dec(ans, 2);
		if ans < 0 then inc(ans, PRIME);
	end;

	writeln(ans);
end.
