program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #string #split #dictionary #counter
uses
	Generics.Collections, SysUtils;
const
	NN = 100;
var
	n, i, c: int32;
	ans: boolean;
	Line: string;
	Tokens: TStringArray;
	s, t: array [1 .. NN] of string;
	cnt: TDictionary<string, int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	cnt := TDictionary<string, int32>.Create;
	for i := 1 to n do begin
		Readln(Line);
		Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
		s[i] := Tokens[0];
		t[i] := Tokens[1];

		if not cnt.TryGetValue(s[i], c) then c := 0;
		cnt.AddOrSetValue(s[i], c+1);
		if not cnt.TryGetValue(t[i], c) then c := 0;
		cnt.AddOrSetValue(t[i], c+1);
	end;

	ans := true;
	for i := 1 to n do
		if s[i] = t[i] then begin
			ans := ans and cnt.TryGetValue(s[i], c) and (c = 2);
		end else begin
			ans := ans and (
				(cnt.TryGetValue(s[i], c) and (c = 1)) or
				(cnt.TryGetValue(t[i], c) and (c = 1)));
		end;
	cnt.Free;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
