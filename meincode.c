#include <stdio.h>
#include <stdlib.h>
#include <math.h>

extern double pow(double, double);

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#define MNV 30              //Komponentenverschiebung im Feld mn
#define MBV 36              //Komponentenverschiebung im Feld mb


double fac[65];             // fac[n] = n!
double binom[65][11];       // binom[n][k] = Binomialkoeffizient von n über k
double choose2 [49][9][9];  // choose[n][j][k] = Multinomialkoeffizient n! / (j! * k! * (n - j - k)!)
double mb [4][29][10][11];  // mb[kpos][s][wq][wr] = Obere Schranke aus Satz 5.3, wobei s + 36 = space
double mn [34][11][11][11]; // mn[n][j][k][l] = Multinomialkoeffizient (n + 30)!/(j! * k! * l! * (n + 30 - j - k - l)!)
double mopps_[9][33][9];    // Umsetzung von m(p,s,n) aus Satz 4.10
double pawns[2][9][9][9];

//Berechnet die Werte von fac[i]
void factorial() {
    int i, j;
    for (i=0; i<65; i++) {
        fac[i] = 1.0;
        for (j=2; j<=i; j++) {
            fac[i] *= j;
        }
    }
}

// Berechnet die Werte von choose2[n][k1][k2]
void choose2function(){
    int n, k1, k2;
    for (n = 0; n <= 48; n++){  
        for (k1 = 0; k1 <= 8; k1++){
            for (k2 = 0; k2 <= 8; k2++){
                if (n >= k1 + k2){
                    choose2[n][k1][k2] = fac[n] / (fac[k1] * fac[k2] * fac[n - k1 -  k2]);
                } else {
                    choose2[n][k1][k2] = 0;
                }
            }
        }
    }
}

//Berechnet die Werte von mopps[p][s][n]
double opps(int p, int s, int n) {
  double res = 0.0;
  if (p == 0 && s == 0) {
    res = 1;
  } else if (p == 0 || n == 0) {
    res = 0;
  } else {
    res = mopps_[p][s][n - 1];
    for (int i = 0; i <= (s < 4 ? s : 4); ++i) { 
      res += (5 - i) * mopps_[p - 1][s - i][n - 1];
    }
  }
  return res;
}

//Weißt mopps[p][s][n], die in opps(p,s,n) berechneten Werte zu
void moppsfunction(){
    double max = 0.0;
    for (int p = 0; p <= 8; ++p) { 
    for (int s = 0; s <= 32; ++s) {
      for (int n = 0; n <= 8; ++n) {
        mopps_[p][s][n] = opps(p, s, n);
      }
    }
  }
}

// Berechnet die innere Summe in Satz 4.12
// dw = 1 <=> weißer Bauer in schwarzer fixierter Spalte
// db = 1 <=> schwarzer Bauer in weißer fixierter Spalte
// ds = ds1 + ds2 = Anzahl an eingeschlossener Felder in fixierten Spalten
double pawnsep (int dw, int db, int ds, int wp, int bp, int opp){ 
    int opp_ = opp - dw - db;                   // Mindestanzahl blockierter nicht-fixierter Spalten
    int s;                                      // Anzahl eingeschlossener Felder
    double res = 0.0;
    for (s = 0; s <= 4 * opp_; s++){
        if(bp + dw >= opp && wp + db >= opp){   // Damit keine negative Komponente von choose2 aufgerufen wird
            res += mopps_[opp_][s][6] * choose2[44 - opp - opp_ - ds - s][wp + db - opp][bp + dw - opp];
        }
    }
    return res;
}


void pawnsfunction(){
    int s, wp, bp, opp;
    int ds1, ds2;
    double res;

    // Fall: Kein Enpassant -> fixp = 0
    // Berechnet obere Schranke aus Satz 4.11
    for (wp = 0; wp <= 8; wp++){
        for (bp = 0; bp <= 8; bp++){
            for (opp = 0; opp <= 8; opp++){
                if (opp > min(wp, bp)){ // Es darf nicht mehr blockierte Spalten als Bauern einer Seite geben
                    pawns[0][wp][bp][opp] = 0;
                } else {
                    res = 0.0;
                    for (s = 0; s <= 4 * opp; s++){
                        res += mopps_[opp][s][8] * choose2[48 - 2 * opp - s][wp - opp][bp - opp];
                    }
                    pawns[0][wp][bp][opp] = res;
                }
            }
        }
    }

    // Fall: Enpassant -> fixp = 1
    // Berechnet obere Schranke aus Satz 4.12
    for (wp = 0; wp <= 8; wp++){
        for (bp = 0; bp <= 8; bp++){
            for (opp = 0; opp <= 8; opp++){
                res = 0;
                if (opp > min(wp + 1, bp +1)){ // Es darf nicht mehr blockierte Spalten als Bauern einer Seite geben
                    pawns[1][wp][bp][opp] = 0;
                } else {
                    res = pawnsep(0, 0, 0, wp, bp, opp);
                    for (ds1 = 0; ds1 <= 2 && opp > 1; ds1++){
                        for (ds2 = 0; ds2 <= 1; ds2++) {
                            res += pawnsep(1, 1, ds1 + ds2, wp, bp, opp);
                        }
                    }
                    for (ds1 = 0; ds1 <= 2 && opp > 0; ds1++){
                        res += pawnsep(1, 0, ds1, wp, bp, opp);
                    }
                    for (ds2 = 0; ds2 <= 1 && opp > 0; ds2++){
                        res += pawnsep(0, 1, ds2, wp, bp, opp);
                    }
                    pawns[1][wp][bp][opp] = res;
                }
            }

        }
    }
}

//Beginn neuer Funktionen

// Berechnet n über k und speichert es in binom[n][k]
void binomial(){
    int i, j;
    for (i=0; i<65; i++) {
        for (j=0; j<11; j++) {
            if (i >= j) {
                binom[i][j] = fac[i] / (fac[j] * fac[i-j]);
            } else {
                binom[i][j] = 0;
            }
        }
    }
}

//Speichert den Wert (n + 30)!/(j! * k! * l! * (n + 30 - j - k - l)!) in mn[n][j][k][l] 
void multinom(){ 
    int spaces, f1, f2, f3; // Spaces wird hier um MNV verschoben
    for (spaces = MNV; spaces <= 63; spaces++){
        for (f1 = 0; f1 <= 10; f1++){
            for (f2 = 0; f2 <= 10; f2++){
                for (f3 = 0; f3 <= 10; f3++){
                    mn[spaces-MNV][f1][f2][f3] = fac[spaces] / (fac[f1] * fac[f2] * fac[f3] * fac[spaces - f1 - f2 - f3]);
                }

            }
        }
    }
}

// Berechnet die obere Schranke aus Lemma 5.3 und speichert sie im Array mb
void multiplebinomrook (){
    int kpos, space, wq, wr;
    int qspaces;
    for (kpos = 0; kpos <= 3; kpos++){
        switch (kpos) {
            case 0: qspaces = 1; break; // Ecke
            case 1: qspaces = 2; break; // Letzte Reihe
            case 2: qspaces = 0; break; // Bauernterritorium 
            case 3: qspaces = 2; break; // Fixierter König
        }
        for (space = MBV; space <= 64; space++){
            for (wq = 0; wq <= 9; wq++){
                for (wr = 0; wr <= 10; wr++){
                    mb[kpos][space-MBV][wq][wr] = binom[space - qspaces - 1][wq] * binom[space - wq - qspaces - 1][wr];
                }
            }
        }
    }
}
// Ende neuer Funktionen

// Die count-Funktion wurde modifiziert, hier werden zur Übersichtlichkeit nur die modifizieren Stellen kommentiert

double count (int fwr, int fbr, int fixp){
    double summe = 0.0;
    double h = 0.0; // Hilfsvariable, damit der Code nicht zu weit nach rechts geht und gedruckt werden kann
    int np = 8 - fixp;
    int fixwk = (fwr != 0 ? 1 : 0);
    int uwr, wk, wq, wnp1, wr, wb, wnp2, wnp3, wn, wnp4, wproms, wp, wpx;
    int ubr, bk, bq, bnp1, br, bb, bnp2, bnp3, bn, bnp4, bproms, bp, bpx;
    int fixbk = (fbr != 0 ? 1 : 0);
    int wpcs, bpcs, caps, maxuwp, maxubp, minopp, space;
    // wprod und bprod werden nicht mehr benötigt, da die Multinomialkoeffizienten mit dem mn-Array berechnet werden
    
    // kpos und kmult kennen wir aus Kapitel 5, multFWK gibt die Anzahl an Möglichkeiten an den weißen König zu platzieren
    // Die Platzierung des weißen Königs ist nach den Bauern, dem schwarzen König, den weißen Damen und weißen nicht-fixierten Türmen
    int kpos, multFWK, kmult;
    uwr = 2 - fwr;
    wk = floor(uwr / 2);

    // Hier wird geschaut, ob es keinen fixierten König gibt.
    if (fbr == 0){

        // Hier werden alle möglichen kpos eines nicht-fixierten Königs durchgegangen
        for (kpos = 0; kpos <= 2; kpos++){
            for (wq = 0; wq <= 1 + np; wq++){
                wnp1 = np - max(wq - 1, 0);
                for (wr = 0; wr <= uwr + wnp1; wr++){
                    wnp2 = wnp1 - max(wr - uwr, 0);
                    for(wb = 0; wb <= 2 + wnp2; wb++){
                        wnp3 = wnp2 - max(wb - 2, 0);
                        for (wn = 0; wn <= 2 + wnp3; wn++){
                            wnp4 = wnp3 - max(wn - 2, 0);
                            wproms = np - wnp4;
                            for (wp = 0; wp <= wnp4; wp++){
                                wpx = np - wp - wproms;
                                wpcs = wk + wq + wr + wb + wn;
                                ubr = 2 - fbr;
                                bk = floor(ubr / 2);
                                for (bq = 0; bq <= 1 + np; bq++){
                                    bnp1 = np - max(bq - 1, 0);
                                    for (br = 0; br <= ubr + bnp1; br++){
                                        bnp2 = bnp1 - max(br - ubr, 0);
                                        for(bb = 0; bb <= 2 + bnp2; bb++){
                                            bnp3 = bnp2 - max(bb - 2, 0);
                                            for (bn = 0; bn <= 2 + bnp3; bn++){
                                                bnp4 = bnp3 - max(bn - 2, 0);
                                                bproms = np - bnp4;
                                                for (bp = 0; bp <= bnp4; bp++){

                                                    bpx = np - bp - bproms;
                                                    bpcs = bk + bq + br + bb + bn;
                                                    caps = 32 - 2 * fixp - fixwk - fixbk - fwr - fbr - wp - bp - wpcs - bpcs;
                                                    maxuwp = bpx + caps - wproms;
                                                    maxubp = wpx + caps - bproms;
                                                    if (maxuwp < 0 || maxubp < 0){
                                                        continue;
                                                    } else {
                                                        minopp = max(0, fixp + wp - maxuwp);
                                                        // Wenn der weiße König fixiert ist, gibt es eine Möglichkeit den weißen König
                                                        // zu platzieren, ansonsten 63 - wr - wq - 4 * fixp - fbr - wp - bp
                                                        space = 64-4*fixp-fwr-fbr-wp-bp-fixwk;
                                                        multFWK = (fwr > 0 ? 1 : space - 1 - wr - wq); 
                                                        h = 0;
                                                        switch (kpos){
                                                            case 0: kmult = 4 - fwr;  break;
                                                            case 1: kmult = 12 - 3 * fixwk - fwr; break;
                                                            case 2: kmult = 48 - 4 * fixp - wp - bp - 3 * fixwk - fwr; break;
                                                        }
                                                        // Der Multinomialkoeffizient von Tromp wird mithilfe des Ergebnisses der oberen
                                                        // Schranke aus Satz 5.3, multFWK und der Verteilung der restlichen Figuren
                                                        // als Multinomialkoeffizient ersetzt 
                                                        h = pawns[fixp][wp][bp][minopp] * mb[kpos][space - MBV][wq][wr] * multFWK;
                                                        h *= kmult *  mn[space - 2 - wq - wr - MNV + fixwk][wn][wb][bq];
                                                        h *= mn[space - 2 - wq - wn - wr - wb - bq - MNV + fixwk][br][bb][bn];
                                                        summe += h;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    } else {
        // Wenn der König fixiert ist, ist in jedem Fall kpos = 3 gegeben und damit auch kmult = 1
        kpos = 3;
        kmult = 1;
        for (wq = 0; wq <= 1 + np; wq++){
            wnp1 = np - max(wq - 1, 0);
            for (wr = 0; wr <= uwr + wnp1; wr++){
                wnp2 = wnp1 - max(wr - uwr, 0);
                for(wb = 0; wb <= 2 + wnp2; wb++){
                    wnp3 = wnp2 - max(wb - 2, 0);
                    for (wn = 0; wn <= 2 + wnp3; wn++){
                        wnp4 = wnp3 - max(wn - 2, 0);
                        wproms = np - wnp4;
                        for (wp = 0; wp <= wnp4; wp++){
                            wpx = np - wp - wproms;
                            wpcs = wk + wq + wr + wb + wn;

                            ubr = 2 - fbr;
                            bk = floor(ubr / 2);
                            np = 8 - fixp;
                            for (bq = 0; bq <= 1 + np; bq++){
                                bnp1 = np - max(bq - 1, 0);
                                for (br = 0; br <= ubr + bnp1; br++){
                                    bnp2 = bnp1 - max(br - ubr, 0);
                                    for(bb = 0; bb <= 2 + bnp2; bb++){
                                        bnp3 = bnp2 - max(bb - 2, 0);
                                        for (bn = 0; bn <= 2 + bnp3; bn++){
                                            bnp4 = bnp3 - max(bn - 2, 0);
                                            bproms = np - bnp4;
                                            for (bp = 0; bp <= bnp4; bp++){

                                                bpx = np - bp - bproms;
                                                bpcs = bk + bq + br + bb + bn;
                                                caps = 32-2*fixp-fixwk-fixbk-fwr-fbr-wp-bp-wpcs-bpcs;
                                                maxuwp = bpx + caps - wproms;
                                                maxubp = wpx + caps - bproms;
                                                if (maxuwp < 0 || maxubp < 0){
                                                    continue;
                                                } else {
                                                    minopp = max(0, fixp + wp - maxuwp);
                                                    space = 64-4*fixp-fwr-fbr-wp-bp-fixwk;
                                                    // Wenn der weiße König fixiert ist, gibt es eine Möglichkeit den weißen König zu
                                                    // platzieren, ansonsten 63 - wr - wq - 4 * fixp - fbr - wp - bp
                                                    multFWK = (fwr > 0 ? 1 : space - 1 - wr - wq);
                                                    h = 0;

                                                    // Der Multinomialkoeffizient von Tromp wird mithilfe des Ergebnisses der oberen
                                                    // Schranke aus Satz 5.3, multFWK und der Verteilung der restlichen Figuren als 
                                                    // Multinomialkoeffizient ersetzt 
                                                    h = pawns[fixp][wp][bp][minopp] * mb[kpos][space - MBV][wq][wr] * multFWK;
                                                    h *= kmult *  mn[space - 2 - wq - wr - MNV + fixwk][wn][wb][bq];
                                                    h *= mn[space - 2 - wq - wn - wr - wb - bq - MNV + fixwk][br][bb][bn];
                                                    summe += h;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return summe;
}

// In main gibt es wenig Unterschiede
// Es werden lediglich mehr Arrays initialisiert und am Ende wird noch ein Vergleich zu Tromps Code gezogen
int main (){

    //Initialisierung der Arrays
    factorial();
    binomial();
    choose2function();
    multinom();
    multiplebinomrook();
    moppsfunction();
    pawnsfunction();
    int fwr, fbr, fixp;
    double res = 0.0;
    double summe = 0.0;
    double multFWR, multEP, multFBR;

    // Alle möglichen Werte für fwr, fbr und fixp durchlaufen
    for (fwr = 0; fwr <= 2; fwr++){
        
        multFWR = (fwr == 1 ? 2 : 1); //multFWR
        
        for (fbr = 0; fbr <= 2; fbr++){

            multFBR = (fbr == 1 ? 2 : 1); //multFBR


            for (fixp = 0; fixp <= 1; fixp++){
     
                multEP = (fixp == 1 ? 14 : 1); //multEP
                
                //Multiplikatoren heranmultiplizieren
                res = multEP * multFWR * multFBR * count(fwr, fbr, fixp);
                printf("fwr = %d\tfbr = %d\tfixp = %d\tgerundet: %e\texakt: %1.0f\n", fwr, fbr, fixp, res, res);
                summe += res;
            }
        }
    }

    // Ergebnis von Tromp
    double haskellergebnis = 8726713169886222032347729969256422370854716254.0;

    printf("Totale Summe: %1.0f \n", 2 * summe);
    printf("Verbesserung um %1.1f Prozent\n", 100 * ((haskellergebnis - 2 * summe)/haskellergebnis));
    return 1;
}
