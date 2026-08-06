program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #permutations
uses
	math, SysUtils;
const
	NN = 8;
var
	n, i: int8;
	ch: char;
	s: string;
	k, p, p2: int32;
	c: array ['a' .. 'z'] of int8;
	f: array [0 .. NN] of int32;
	Line: string;
	Tokens: TStringArray;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(Line);
	Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
	
	s := Tokens[0];
	k := StrToInt(Tokens[1]);
	n := length(s);

	for ch := 'a' to 'z' do c[ch] := 0;

	f[0] := 1;
	for i := 1 to n do begin
		inc(c[s[i]]);
		f[i] := f[i-1] * i;
	end;

	p := f[n];
	for ch := 'a' to 'z' do p := p div f[c[ch]];

	for i := 1 to n do begin
		s[i] := 'a';
		p2 := p * c[s[i]] div (n+1-i);

		while (s[i] <= 'z') and (k > p2) do begin
			dec(k, p2);
			inc(s[i]);
			p2 := p * c[s[i]] div (n+1-i);
		end;

		p := p2;
		dec(c[s[i]]);
	end;

	writeln(s);
end.
