program Program_Comparer;
{$MODE DELPHI}
uses
    Generics.Defaults, Generics.Collections;
const
    nn = 100 * 1000;
type
    TIntComparer = class(TComparer<int32>)
        function Compare(constref Left, Right: int32): Integer; override;
    end;
var
    n, i: int32;
    Comparer: TIntComparer;
    a: TList<int32>;

function TIntComparer.Compare(constref Left, Right: int32): Integer;
begin
    Result := Left - Right;
end;

begin
    n := nn div 10;

    Comparer := TIntComparer.Create;
    Comparer._AddRef;
    a := TList<int32>.Create(Comparer);

    for i := 0 to nn do a.Add(random(nn));
    for i := 0 to 10 do write(' ', a[i*n]); writeln;

    a.Sort;
    for i := 0 to 10 do write(' ', a[i*n]); writeln;
    writeln(a.Count);

    a.Free;
    {Comparer.Free;}
    Comparer._Release;
end.
(*
 54881 36678 74826 38613 39217 88120 75812 79091 36925 50366 30764
 0 9857 19828 29869 39999 49890 59703 69823 79858 89858 99999
100001

=====
Used: 78 ms, 44 KB
*)
