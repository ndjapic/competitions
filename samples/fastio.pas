program fastio;
{$MODE DELPHI}{$H+}{$INLINE ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
const
	nn = 200 * 1000;
type
	TIntComparer = class(TComparer<int32>)
		function Compare(constref Left, Right: int32): Integer; override;
	end;
var
	notc, tci: int32;
	n, i: int32;
	Split: TStringList;
	Line : string;
	Comparer: TIntComparer;
	a: TList<int32>;
	ans: array [0 .. nn] of int32;

function TIntComparer.Compare(constref Left, Right: int32): Integer;
begin
	Result := Left - Right;
end;

begin
	Split := TStringList.Create;
	Split.Clear;
	Split.Delimiter := ' ';

	Comparer := TIntComparer.Create;
	Comparer._AddRef;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		readln(Line);
		Split.DelimitedText := Line;

		a := TList<int32>.Create(Comparer);
		for i := 0 to n-1 do a.Add(StrToInt(Split[i]));
		a.Sort;

		for i := 0 to a.Count -1 do begin
			ans[i] := sqr(a[i]);
			Split[i] := IntToStr(ans[i]);
		end;

		writeln(Split.DelimitedText);
		flush(StdErr); flush(output); // DO NOT REMOVE
		FreeAndNil(a);

	end;

	FreeAndNil(Split);
	{Comparer.Free;}
	Comparer._Release;
end.
(*
	1
	5
	2  5  3  4  1  
	1 4 9 16 25


	------------------
	(program exited with code: 0)
	Press return to continue
* )
