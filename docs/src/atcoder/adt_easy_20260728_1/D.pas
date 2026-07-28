program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	NN = 200 * 1000;
var
	n, i, k, ans: int32;
	l: array [1 .. NN] of int32;
	a: array [1 .. NN] of array of int32;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function cmp(constref i, j: int32): int32;
var
	k: int32;
begin
	result := CompareValue(l[i], l[j]);
	if result = 0 then begin
		k := 0;
		while (k < l[i] - 1) and (a[i][k] = a[j][k]) do inc(k);
		result := CompareValue(a[i][k], a[j][k]);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);
	p := TList<int32>.Create;

	for i := 1 to n do begin
		read(l[i]);
		setlength(a[i], l[i]);

		for k := 0 to l[i] - 1 do read(a[i][k]);
		readln;

		p.Add(i);
		p.Exchange(i-1, Random(i));
	end;
	p.Sort(TComparer<int32>.Construct(cmp));

	ans := 1;
	for i := 1 to n-1 do
		if cmp(p[i-1], p[i]) < 0 then inc(ans);

	writeln(ans);
	p.Free;
end.
