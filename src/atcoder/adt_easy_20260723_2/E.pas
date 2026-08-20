program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	at, bt, d: int32;
	ch: char;
	s, t: string;
	a, b: array ['a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for ch := 'a' to 'z' do begin
		a[ch] := 0;
		b[ch] := 0;
	end;
	at := 0;
	bt := 0;

	readln(s);
	readln(t);

	for ch in s do
		if ch = '@' then
			inc(at)
		else
			inc(a[ch]);

	for ch in t do
		if ch = '@' then
			inc(bt)
		else
			inc(b[ch]);

	for ch in 'atcoder' do
		if a[ch] < b[ch] then begin
			d := min(at, b[ch] - a[ch]);
			dec(at, d);
			inc(a[ch], d);
		end else if b[ch] < a[ch] then begin
			d := min(bt, a[ch] - b[ch]);
			dec(bt, d);
			inc(b[ch], d);
		end;

	ch := 'a';
	while (ch <= 'z') and (a[ch] = b[ch]) do inc(ch);

	if ch > 'z' then
		writeln('Yes')
	else
		writeln('No');
end.
