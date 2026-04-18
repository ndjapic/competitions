# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections;
const
	ee = 5;
	ii = 31;
var
	e, i: int32;
	a: array [0 .. ee] of int32;
	score: array [1 .. ii] of int32;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareP(constref Left, Right: int32): Integer;
begin
	Result := score[Right] - score[Left];
	if Result = 0 then Result := Right - Left;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	for e := ee-1 downto 0 do read(a[e]); readln;

	p := TList<int32>.Create;
	for i := 1 to ii do begin
		score[i] := 0;
		for e := ee-1 downto 0 do
			if odd(i shr e) then inc(score[i], a[e]);
		p.Add(i);
		p.Exchange(i-1, random(i));
	end;
	p.Sort(TComparer<int32>.Construct(CompareP));

	for i in p do begin
		for e := ee-1 downto 0 do
			if odd(i shr e) then write(chr(ord('E') - e));
		writeln;
	end;

	p.Free;
end.

```
