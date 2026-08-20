program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, SysUtils;
const
	NN = 100;
var
	n, i, c: int8;
	ans: boolean;
	s, t: array [1 .. NN] of string;
	Line: string;
	Tokens: TStringArray;
	d: TDictionary<string, int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(n);

	d := TDictionary<string, int8>.Create;
	for i := 1 to n do begin
		Readln(Line);
		Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);

		s[i] := Tokens[0];
		if not d.TryGetValue(s[i], c) then c := 0;
		d.AddOrSetValue(s[i], c+1);

		t[i] := Tokens[1];
		if not d.TryGetValue(t[i], c) then c := 0;
		d.AddOrSetValue(t[i], c+1);
	end;

	i := 0;
	ans := true;
	repeat
		inc(i);
		if s[i] = t[i] then
			ans := d.TryGetValue(s[i], c) and (c = 2)
		else
			ans := (
				d.TryGetValue(s[i], c) and (c = 1)
			) or (
				d.TryGetValue(t[i], c) and (c = 1)
			);
	until (i >= n) or not ans;

	if ans then
		writeln('Yes')
	else
		writeln('No');

	d.Free;
end.
