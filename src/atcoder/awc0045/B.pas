program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
type
	TIntArray = array of Integer;
var
	n, m, i, j: int32;
	a, b: TIntArray;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function KMP_Search(const Text, Pattern: TIntArray): Integer;
var
	n, m, i, j: Integer;
	Next: TIntArray;
begin
	n := Length(Text);
	m := Length(Pattern);
	Result := -1;

	SetLength(Next, m);
	Next[0] := 0;
	j := 0;
	for i := 1 to m - 1 do begin
		while (j > 0) and (Pattern[i] <> Pattern[j]) do j := Next[j - 1];
		if Pattern[i] = Pattern[j] then Inc(j);
		Next[i] := j;
	end;

	i := 0;
	j := 0;
	while i < n do begin
		while (j > 0) and (Text[i] <> Pattern[j]) do j := Next[j - 1];
		if Text[i] = Pattern[j] then Inc(j);

		if j = m then begin
			Result := i - m + 1;
			i := n;
		end else
			inc(i);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	setlength(a, n);
	setlength(b, m);

	for i := 0 to n-1 do read(a[i]); readln;
	for j := 0 to m-1 do read(b[j]); readln;

	i := KMP_Search(a, b);
	if i > -1 then inc(i);

	writeln(i);
end.
