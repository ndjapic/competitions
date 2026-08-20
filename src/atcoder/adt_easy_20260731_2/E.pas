program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	SysUtils;
const
	NN = 500 * 1000;
var
	n, i, j, k, m, l, r: int32;
	err: boolean;
	s, t, Line: string;
	Tokens: TStringArray;
	ans: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;


begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(Line);
	Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
	n := StrToInt(Tokens[0]);
	t := Tokens[1];
	m := length(t);

	k := 0;
	for i := 1 to n do begin
		readln(s);

		if abs(length(s) - m) <= 1 then begin
			if length(s) < m then begin
				l := 1;
				r := m-1;
				while (l <= r) and (s[l] = t[l]) do inc(l);
				while (l <= r) and (s[r] = t[r+1]) do dec(r);
				err := l > r;
			end else if length(s) > m then begin
				l := 1;
				r := m;
				while (l <= r) and (s[l] = t[l]) do inc(l);
				while (l <= r) and (s[r+1] = t[r]) do dec(r);
				err := l > r;
			end else begin
				l := 1;
				r := m;
				while (l <= r) and (s[l] = t[l]) do inc(l);
				while (l <= r) and (s[r] = t[r]) do dec(r);
				err := l >= r;
			end;

			if err then begin
				inc(k);
				ans[k] := i;
			end;
		end;
	end;

	writeln(k);
	for j := 1 to k do begin
		write(ans[j]);
		if j < k then write(' ')
	end;
	writeln;
end.
