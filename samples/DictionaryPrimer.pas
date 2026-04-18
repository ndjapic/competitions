program DictionaryPrimer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, Generics.Collections;

var
  Mapa: TDictionary<string, Integer>;
  Kljuc: string;
  Par: TPair<string, Integer>; // Tip za for-in petlju

begin
  // 1. Inicijalizacija
  Mapa := TDictionary<string, Integer>.Create;
  {Mapa.Capacity := 100000;}
  try
    // 2. Dodavanje i ažuriranje (Items[] radi oba bezbedno)
    Mapa.AddOrSetValue('Srbija', 10);
    Mapa['Grcka'] := 5;
    Mapa['Srbija'] := 15; // Ažuriranje postojeće vrednosti

    // 3. Provera postojanja i čitanje (TryGetValue je najbrži)
    if Mapa.ContainsKey('Srbija') then
      WriteLn('Srbija postoji sa vrednošću: ', Mapa['Srbija']);

    // 4. Prolaz kroz sve elemente (Iteracija)
    WriteLn('--- Sadržaj rečnika ---');
    for Par in Mapa do
      WriteLn(Par.Key, ': ', Par.Value);

    // 5. Brisanje
    Mapa.Remove('Grcka');

    // 6. Broj elemenata
    WriteLn('Preostalo elemenata: ', Mapa.Count);

    // 7. Pražnjenje
    Mapa.Clear;

  finally
    // 8. Oslobađanje memorije (Obavezno!)
    Mapa.Free;
  end;
  
  ReadLn;
end.


uses System.Generics.Collections, System.Generics.Defaults;

var
  ListaKljuceva: TList<string>;
  Kljuc: string;
begin
  // Prebacivanje ključeva u listu
  ListaKljuceva := TList<string>.Create;
  for Kljuc in Mapa.Keys do
    ListaKljuceva.Add(Kljuc);

  // Sortiranje (podrazumevano rastuće)
  ListaKljuceva.Sort;

  // Ispis po redosledu
  for Kljuc in ListaKljuceva do
    WriteLn(Kljuc, ': ', Mapa[Kljuc]);

  ListaKljuceva.Free;
end;


type
  TSigurniHash = class(TInterfacedObject, IEqualityComparer<Integer>)
    function Equals(const Left, Right: Integer): Boolean;
    function GetHashCode(const Value: Integer): Integer;
  end;

function TSigurniHash.Equals(const Left, Right: Integer): Boolean;
begin
  exit(Left = Right);
end;

function TSigurniHash.GetHashCode(const Value: Integer): Integer;
begin
  // Dodavanje nasumičnog XOR-a (seed) otežava predviđanje kolizija
  // Seed bi trebalo da bude neka nasumična vrednost dobijena na početku (npr. Random(MaxInt))
  exit(Value xor 123456789); 
end;

// Upotreba:
var
  Mapa: TDictionary<Integer, Integer>;
begin
  Mapa := TDictionary<Integer, Integer>.Create(TSigurniHash.Create);
  // ... dalje se koristi normalno ...
end;


program MultimapPrimer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, Generics.Collections;

type
  // Definisanje tipa radi lakšeg čitanja
  TListaVrednosti = TList<Integer>;
  TMultimap = TDictionary<string, TListaVrednosti>;

var
  Mapa: TMultimap;
  Lista: TListaVrednosti;
  Kljuc: string;
  Vrednost: Integer;

procedure DodajVrednost(AMapa: TMultimap; const AKljuc: string; AVrednost: Integer);
var
  L: TListaVrednosti;
begin
  // Proveravamo da li lista za taj ključ već postoji
  if not AMapa.TryGetValue(AKljuc, L) then
  begin
    // Ako ne postoji, kreiramo novu listu i dodajemo je u rečnik
    L := TListaVrednosti.Create;
    AMapa.Add(AKljuc, L);
  end;
  // Dodajemo vrednost u listu (bilo da je nova ili stara)
  L.Add(AVrednost);
end;

begin
  Mapa := TMultimap.Create;
  {TObjectDictionary<string, TList<Integer>>.Create([doFreeOnRelease])}
  try
    // Dodavanje vrednosti
    DodajVrednost(Mapa, 'A', 10);
    DodajVrednost(Mapa, 'A', 20);
    DodajVrednost(Mapa, 'B', 50);

    // Iteracija kroz multimap
    for Kljuc in Mapa.Keys do
    begin
      Write(Kljuc, ': ');
      for Vrednost in Mapa[Kljuc] do
        Write(Vrednost, ' ');
      WriteLn;
    end;

{Lista.BinarySearch(vrednost, index)}

  finally
    // KRITIČNO: Prvo moramo obrisati svaku listu pojedinačno, pa onda rečnik
    for Lista in Mapa.Values do
      Lista.Free;
    Mapa.Free;
  end;

  ReadLn;
end.
