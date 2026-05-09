program TestGenerics;

{$mode delphi}

uses
  Generics.Collections;

var
  // Тестирање доступности структура
  MS: TTreeMultiset<Integer>; 
  TS: TTreeSet<Integer>;
  D: TDictionary<Integer, Integer>;

begin
  try
    // Тест за Multiset (идеалан за дупликате + сортирање)
    MS := TTreeMultiset<Integer>.Create;
    MS.Add(10);
    MS.Add(10);
    WriteLn('TTreeMultiset је доступан. Број елемената: ', MS.Count);
    MS.Free;
  except
    WriteLn('TTreeMultiset НИЈЕ доступан.');
  end;

  try
    // Тест за TreeSet (само јединствени + сортирање)
    TS := TTreeSet<Integer>.Create;
    WriteLn('TTreeSet је доступан.');
    TS.Free;
  except
    WriteLn('TTreeSet НИЈЕ доступан.');
  end;
end.
