# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
type
	TBits = array of uint64;
var
	h, w, i, j, bcount: int32;
	s: string;
	b, b2: array of TBits;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function compare(l, r: TBits): int32;
var
	i: int32;
begin
	i := 0;
	while (i < bcount) and (l[i] = r[i]) do inc(i);
	if l[i] < r[i] then
		compare := -1
	else if l[i] > r[i] then
		compare := +1
	else
		compare := 0;
end;

procedure merge(l, m, r: int32);
var
	i, j, k: int32;
begin
	i := l;
	j := m;
	for k := l to r-1 do
		if (j = r) or (i < m) and (
			compare(b[i], b[j]) <= 0
		) then begin
			b2[k] := b[i];
			inc(i);
		end else begin
			b2[k] := b[j];
			inc(j);
		end;
	for k := l to r-1 do b[k] := b2[k];
end;

procedure msort(l, r: int32);
var
	m: int32;
begin
	if r-l > 1 then begin
		m := (l+r) div 2;
		msort(l, m);
		msort(m, r);
		merge(l, m, r);
	end;
end;

procedure switch(i, j: int32);
begin
	b[j][i div 64] := b[j][i div 64] xor (uint64(1) shl (i mod 64));
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);
	bcount := h div 64;
	setlength(b, 2*w);
	setlength(b2, 2*w);

	for j := 0 to 2*w-1 do begin
		setlength(b[j], bcount + 1);
		for i := 0 to bcount do b[j][i] := 0;
	end;

	for i := 0 to h-1 do begin
		readln(s);
		for j := 0 to w-1 do
			if s[j+1] = '#' then switch(i, j);
	end;

	for i := 0 to h-1 do begin
		readln(s);
		for j := 0 to w-1 do
			if s[j+1] = '#' then switch(i, j+w);
	end;

	msort(0, w);
	msort(w, 2*w);

	j := 0;
	while (j < w) and (compare(b[j], b[j+w]) = 0) do inc(j);

	if j = w then
		writeln('Yes')
	else
		writeln('No');
end.

```
