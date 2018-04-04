#include <fstream>
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include<string>
#include<sstream>
#include "spline.h"
using namespace std;

const int nr = 150;
const double devallow = 0.00001;

struct isoparameter
{
  double b;
  double aga;
  double bga;
};

string itos(int i)
{
    stringstream s;
    s << i;
    return s.str();
}

double trybvalue(double, double);
isoparameter makeisoequ(double, double);

int main(int argc, char* argv[])
{
  const int nr = 100, nout = 1001;
  double ratio, q0target;
  int i1, i2, i3;
  double eps;
  double yita1, shift1, q01, a01, beta1, betap1, li1, delta1, current1, psi1,axi1,elongaxis1;
  double yita , shift , q0 , a0 , beta , betap , li , delta ,current, psi,axi,elongaxis;
  double elongbound, triabound, epsbound;
  double psi0, tpar, tavg, pavg, det, rbphi2, tper, te, boundlen, pper;
  double rhotemp, ppar;
  double tpar0, h0, f0, b, hot;
  double tpart, hh, ff;
  double junk;
  double ps[nr], rho[nr], p[nr], f2[nr], t[nr], bl[nr];
  ifstream fin;
  ofstream fout, foutdat;

  vector<double> psv(nr),  hv(nr), f2v(nr), tv(nr), thetav(nr);
  tk::spline hspline, f2spline, tspline, thetaspline;
    
  string outfn;
  string outmp;
  string outin;
  string outq;
  string outaxis;

  cin  >> eps;
  foutdat.open("global.dat");
  elongbound = 1.0;
  triabound = 0.0;
  epsbound = 0.25062;

  isoparameter isodata;
  // system("cp fort.10 namelist/bussac1ani");

  isodata.aga = 0;
  isodata.bga = 0;
  isodata.b = 0.005;
  if (eps==0) return 1;
  for (i3 = 0; i3 <= 0; i3++)
    {

      system("cp isotropic fort.10");
      system("./helcode");
      // system("cp fort.20 org_output");
      // system("cp fort.12 org_map");

      fin.open("fort.22");
      fin >> axi1 >> betap1 >> beta1 >> li1 >> delta1 >> current1 >> psi1 >>a01 >> yita1 >> shift1 >> elongaxis1 >> q01;
      fin.close();
      fin.open("fort.37");
      for (i1 = 0; i1 < nr; i1++)
	{
	  fin >> ps[i1] >> rho[i1] >> junk >> p[i1] >> junk >> f2[i1] >> bl[i1];
	  p[i1] = p[i1] * psi1 * psi1;
	  t[i1] = p[i1] / rho[i1];
	  f2[i1] = f2[i1] * psi1 * psi1;
	}
      fin.close();
  
      for (i2 =0; i2 <10  ; i2++)
	{
	  // Read Global parameters
	  fin.open("fort.22");
	  fin >>  axi >> betap >> beta >> li >> delta >> current >> psi >> a0 >> yita >> shift >> elongaxis >> q0;
	  fin.close();

	  // Read Old profile
	  fin.open("fort.37");
	  fout.open("fort.10");
	  fin >> psi0 >> rhotemp >> ppar >> pavg >> det >> rbphi2 >> boundlen;
	  pavg = pavg * psi * psi;
	  ppar = ppar * psi * psi;
	  tavg = pavg / rhotemp;
	  tpar = ppar / rhotemp;
	  rbphi2 = rbphi2 * psi;
	  tper = (3.0 * tavg - tpar) / 2.0;
	  pper = tper * rhotemp;
	  te = (2.0*tper+tpar)/3.0;
	  tpar0 = t[0] / (te / tpar);
	  h0 = tpar0 * log(tpar/tper * rho[0]);
	  f0 = f2[0] * (1-4.0/3.0*det);
	  hot =    h0 / tpar0;
	  b = tpar0 / f0;
	  fout.setf(ios::scientific);
	  fout << setprecision(10);
	  fout << "&NUM      NR =51, NP =33,  NRMAP=100,  NPMAP =101, NCHI=129, NITER =200, &END" << endl;
	  fout << "&PLOT NPL1 = 1 &END" << endl;
	  fout << "&PHYS EPS = " << epsbound << ", BVAC=1., RVAC=3., THTOF = " << eps << ", ";
	  fout << "B = " << b << ", HOT = " << hot << " &END" << endl;

	  fout << "&PROFILE IGAM = 3, IPAI = 3, ITE = 3, NPTS = " << nout << "," << endl;
	  fout << "ITH   =1, ATH=0.,BTH=0.,CTH=10. ,"<<endl;
	  //fout << "VH(1) =1.000000e+00, VTE(1) =1.000000e+00, VF2( 1) =1.000000e+00," <<endl;

	  psv[0] = 0.0;
	  f2v[0] = hv[0] = tv[0] = 1.0;
	  for (i1 = 1; i1 < nr; i1++)
	    {
	      fin >> psv[i1] >> rhotemp >> ppar >> pavg >> det >> rbphi2 >> boundlen;
	      psv[i1] = sqrt(psv[i1]);
	      pavg = pavg * psi * psi;
	      ppar = ppar * psi * psi;
	      tavg = pavg / rhotemp;
	      tpar = ppar / rhotemp;
	      rbphi2 = rbphi2 * psi * psi;
	      tper = (3.0 * tavg - tpar) / 2.0;
	      pper = tper * rhotemp;
	      te = (2.0*tper+tpar)/3.0;
	      tpart = t[i1] / (te / tpar);
	      hh = tpart * log(tpar/tper * rho[i1]);
	      if (tpart == 0.0)
		{
		  hh = 0.0;
		}
	      ff = f2[i1] * (1-4.0/3.0*det);
	      hv[i1] = hh/h0;
	      f2v[i1] = ff/f0;
	      tv[i1] = tpart/tpar0;
	    }
	  hspline.set_points(psv, hv);
	  f2spline.set_points(psv, f2v);
	  tspline.set_points(psv, tv);
	  for (i1 = 0; i1 < nout; i1++)
	    {
	      junk = 1. / double(nout-1) * double(i1);
	      if ((h0 < 1e-8) && (h0 > -1e-8))
		{
		  fout << "VH("   << i1+1 << ") =" << 1.0 << ", ";
		}
	      else
		{
		  fout << "VH("   << i1+1 << ") =" << hspline(junk) << ", ";
		}
	      a0 = tspline(junk);
	      if (a0 < 1e-4)
		{
		  fout << "VTE("  << i1+1 << ") =" <<1e-4 << ", ";
		}
	      else
		{
		  fout << "VTE("  << i1+1 << ") =" <<a0 << ", ";
		}
	      fout << "VF2( " << i1+1 << ") =" << f2spline(junk) << ", ";
	      fout << endl;	    }
	  fout  << "&END" << endl;
  
	  fout << " &SHAPE ELLIP= " << elongbound << ",TRIA= " << triabound << ", MHARM= 64,ISHAPE=  1,ISOL=  0,MFM=128,IAS=0,IMESH =2,XR1=0.95, SIG1 =0.2, &END" << endl;
  
	  fin.close();
	  fout.close();
	  system("./helcode");
	}
      fin.open("fort.22");
      fin >>  axi >> betap >> beta >> li >> delta >> current >> psi >> a0 >> yita >> shift >> elongaxis >> q0;
      fin.close();
      foutdat.setf(ios::scientific);
      foutdat << axi << ' ';
      foutdat <<delta1<< ' ' <<delta<< ' ' << shift1 << ' ' << shift << ' ';
      foutdat << q01  << ' ' << q0  << ' ' << betap1 << ' ' << betap << ' ';
      foutdat << beta1<< ' ' << beta<< ' ' << li1    << ' ' << li    << ' ';
      foutdat << psi1 << ' ' << psi << ' ' << current1 << ' ' << current << ' ';
      foutdat << elongaxis1 << ' ' << elongaxis << endl;
      outfn = string("cp fort.20 scan/");
      outfn += itos(i3+6);
      outfn += string(".out");

      outmp = string("cp fort.12 scan/");
      outmp += itos(i3+6);
      outmp += string(".map");

      outfn = string("cp fort.20 scan/");
      outfn += itos(i3+6);
      outfn += string(".out");

      outin = string("cp fort.10 scan/");
      outin += itos(i3+6);
      outin += string(".in");

      outq = string("cp fort.34 scan/");
      outq += itos(i3+6);
      outq += string(".q");

      outaxis = string("cp fort.35 scan/");
      outaxis += itos(i3+6);
      outaxis += string(".axis");

      system(outfn.c_str());
      system(outmp.c_str());
      system(outin.c_str());
      system(outq .c_str());
      system(outaxis.c_str());
    }
  foutdat.close();
  system("cp fort.10 namelist/helbal");
  system("./runhel helbal");

}

isoparameter makeisoequ(double ratio, double q0target)
{
  double b1, b2, bmid;
  double qleft, qright, qmid;
  int i1 = 0;

  b1 = 0.007;
  b2 = 0.003;
  
  qleft = trybvalue(b1, ratio);
  qright = trybvalue(b2, ratio);

  cout << "+++++++++++Starting iteration+++++++++++"<<endl;
  cout << "qleft = " << qleft << ", qright = " << qright << ", qtarget = " << 
    q0target << endl;

  do
    {
      bmid = (b1 + b2)/2.0;
      qmid = trybvalue(bmid,ratio);
      if (qmid < q0target)
  	{
  	  qleft = qmid;
  	  b1 = bmid;
  	}
      else
  	{
  	  qright = qmid;
  	  b2 = bmid;
  	}
      i1++;
      cout << "Iteration " << i1 <<", qmid = " << qmid << endl;
    }while ((((q0target-qmid)>devallow) || ((q0target-qmid)<-devallow)) && (i1 < 20));

  isoparameter output;
  output.b = bmid;
  output.aga = - 2.0/ratio * bmid;
  output.bga = - 0.5 * output.aga + bmid;

  return output;
}

double trybvalue(double btry, double ratio)
{
  double eps, elongbound, triabound, epsbound;
  double yita1, shift1, q01, a01, beta1, betap1, li1, delta1, current1, psi1,axi1,elongaxis1;
  double alpha1, alpha2;
  ifstream fin;
  ofstream fout;

  eps = -0.5;
  elongbound = 1.0;
  triabound = 0.0;
  epsbound = 0.1;

  alpha1 = - 2.0/ratio * btry;
  alpha2 = - 0.5 * alpha1 + btry;
  
  fout.open("fort.10");
  fout.setf(ios::scientific);
  fout <<"&NUM NR    =51, NP    =33, NRMAP =100, NPMAP =101, NCHI  =129, NITER =200, NMESH =1, &END" << endl;
  fout << "&PLOT NPL1 = 1, &END" << endl;
  fout << "&PHYS EPS = " << epsbound << ", BVAC  =1.0, RVAC=3.0 , ";
  fout << "B = " << btry << ", HOT = 0.0, THTOF =" << eps << " &END" << endl;

  fout << "&PROFILE  IGAM  = 1, AGA=" << alpha1 << ",BGA=" << alpha2 << "," << endl;
  fout << "IPAI  = 1, API=-1.," << endl;
  fout << "ITE   = 1, ATE=-1.,"<< endl;
  fout << "ITH   =0, ATH=0.,BTH=0.,CTH=10.  &END"<<endl;
  fout << " &SHAPE ELLIP= " << elongbound << ",TRIA= " << triabound << ", MHARM= 128,ISHAPE=  1,ISOL=  0,MFM=256,IAS=0,IMESH =0, &END" << endl;
  fout.close();
      
  system("./helcode > tmpout.out");

  fin.open("fort.22");
  fin >> axi1 >> betap1 >> beta1 >> li1 >> delta1 >> current1 >> psi1 >> q01;
  fin.close();
  return q01;
}
