# Задатак: Pattern_BMH.pas

```pascal
program Pattern_BMH;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections;
type
	TIntArray = array of Integer;
var
	n, m, i, j: int32;
	a, b: TIntArray;

function BoyerMooreHorspool(const Text, Pattern: TIntArray): Integer;
var
	n, m, i, j: Integer;
	Skip: array of Integer;
	const MAX_INT_VALUE = 65535; // Прилагодите опсегу ваших бројева ако је потребно
begin
	Result := -1; // Враћа -1 ако није пронађено
	n := Length(Text);
	m := Length(Pattern);

	{ 1. Припрема табеле скокова (Preprocessing) }
	{ Користимо речник или мапу ако су цели бројеви ван малих граница. 
		За опсег малих бројева, обичан низ је најбржи. }
	SetLength(Skip, MAX_INT_VALUE + 1); 
	for i := 0 to MAX_INT_VALUE do Skip[i] := m;
	for i := 0 to m - 2 do Skip[Pattern[i]] := m - 1 - i;

	{ 2. Претрага }
	i := 0;
	while i <= n - m do begin
		j := m - 1;
		{ Поредимо сдесна налево }
		while (j >= 0) and (Text[i + j] = Pattern[j]) do Dec(j);

		if j < 0 then begin
			Result := i; // Пронађено прво појављивање
			Exit;
		end;

		{ Скок на основу последњег елемента тренутног прозора }
		i := i + Skip[Text[i + m - 1]];
	end;
end;

function BMH_LargeRange(const Text, Pattern: array of Integer): Integer;
var
	n, m, i, j, skipVal: Integer;
	SkipMap: TDictionary<Integer, Integer>;
begin
	Result := -1;
	n := Length(Text);
	m := Length(Pattern);

	SkipMap := TDictionary<Integer, Integer>.Create;
	try
		for i := 0 to m - 2 do SkipMap.AddOrSetValue(Pattern[i], m - 1 - i);

		i := 0;
		j := m - 1;
		while (i <= n - m) and (j >= 0) do begin
			while (j >= 0) and (Text[i + j] = Pattern[j]) do Dec(j);

			if j >= 0 then begin
				if not SkipMap.TryGetValue(Text[i + m - 1], skipVal) then skipVal := m;
				i := i + skipVal;
				j := m - 1;
			end else
				Result := i;
		end;
	finally
		SkipMap.Free;
	end;
end;

begin
	readln(n, m);

	setlength(a, n);
	setlength(b, m);

	for i := 0 to n-1 do read(a[i]); readln;
	for j := 0 to m-1 do read(b[j]); readln;

	i := BMH_LargeRange(a, b);
	if i > -1 then inc(i);

	writeln(i);
end.

```
