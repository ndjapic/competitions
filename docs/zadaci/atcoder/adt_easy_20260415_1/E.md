# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, Classes, SysUtils;
const
	nn = 8;
var
	n, i: int8;
	k: int32;
	ios, s: string;
	Map: TDictionary<string, boolean>;
	KeysList: TList<string>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(i: int8);
var
	j: int8;
	ch: char;
begin
	if i <= n then begin
		dfs(i+1);
		for j := i+1 to n do begin
			ch := s[i];
			s[i] := s[j];
			s[j] := ch;

			dfs(i+1);

			ch := s[i];
			s[i] := s[j];
			s[j] := ch;
		end;
	end else
		Map.AddOrSetValue(s, true);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ios);

	n := length(ios);
	i := pos(' ', ios);
	k := StrToInt(RightStr(ios, n-i));

	n := i-1;
	s := LeftStr(ios, n);

	Map := TDictionary<string, boolean>.Create;
	dfs(1);

	KeysList := TList<string>.Create;
	for s in Map.Keys do KeysList.Add(s);

	KeysList.Sort;
	writeln(KeysList[k-1]);

	Map.Free;
	KeysList.Free;
end.

```
