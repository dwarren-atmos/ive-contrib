/* ************************************************************************** */
/*                                                                            */
/*                                archiver.c                                  */
/*                                                                            */
/* ************************************************************************** */
 
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* RCS id string                                                              */
   const static char rcs_id_string[] = 
    "$Id: archiver.c,v 1.6 2001/10/30 09:35:39 graziano Exp $";
/*                                                                            */
/* -------------------------------------------------------------------------- */
 
/* ************************************************************************** */
/* ***********************  UNIVERSITA' DEGLI STUDI  ************************ */
/* ******************************   L'AQUILA   ****************************** */
/* ************************************************************************** */
/*                                                                            */
/*                   Dipartimento di Fisica dell'Atmosfera                    */
/*                                                                            */
/* PROJECT    : PSTd'A 2001                                                   */
/* FILENAME   : archiver.c                                                    */
/* AUTHOR     : Graziano Giuliani                                             */
/* DESCRIPTION: Archives MM5 file in NetCDF format                            */
/* DOCUMENTS  : See README file                                               */
/*                                                                            */
/* ************************************************************************** */
 
/* -------------------------------------------------------------------------- */
/*  HEADER FILES                                                              */
/* -------------------------------------------------------------------------- */
#include <pwd.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>
#include <netdb.h>
#include <string.h>
#include <libgen.h>
#include <netcdf.h>
#include <utils.h>
#include <mm5_io.h>

/* -------------------------------------------------------------------------- */
/*  DEFINITIONS                                                               */
/* -------------------------------------------------------------------------- */
#define INSTITUTION "CETEMPS, L'Aquila, Italy"
#define STARTSTEP  6
#define NUMSTEPS  24
#define CHECK_ERROR(nc_stat) \
  if (nc_stat != NC_NOERR) \
  { \
    fprintf(stderr, "Error netcdf = %s\n", nc_strerror(nc_stat)); \
    return(-1); \
  }

typedef struct {
  int nc;
  int mm5;
  int notimedep;
} t_myvar;

/* -------------------------------------------------------------------------- */
/*  LOCAL FUNCTION PROTOTYPES                                                 */
/* -------------------------------------------------------------------------- */
static int my_def_var(int ncid, nc_type type, int ndim, int *rank,
               char *name, char *long_name, char *units);

static struct tm *inctime(time_t reftime, int inc);

/* -------------------------------------------------------------------------- */
/* PROGRAM     : archiver                                                     */
/* DESCRIPTION : Convert MM5 output files for archiving                       */
/* IN DATA     : Command line arguments :                                     */
/*               Name of MM5 file                                             */
/* OUT DATA    : none                                                         */
/* RETURN      : 0 on completion, -1 otherwise                                */
/* -------------------------------------------------------------------------- */
int main(int argc, char *argv[])
{
  int tstart, tstop;
  t_mm5_file file;
  t_mm5_var *this;
  t_mm5_option opt;
  struct tm *actime;
  char *charp;
  char *charp1;
  char *charp2;
  float *value;
  float *vlevs;
  float *pstar;
  float *pstard;
  float xtime;
  char outfilename[256];
  char timeunit[64];
  int ncid;
  int dimtime;
  int dimidot;
  int dimjdot;
  int dimicross;
  int dimjcross;
  int dimsigmahalf;
  int dimsigmafull;
  int vtime;
  int vsigmahalf;
  int vsigmafull;
  int vc_dim_i;
  int vc_dim_j;
  int vdim_i;
  int vdim_j;
  int vexp_flag;
  int vexp_dim_i;
  int vexp_dim_j;
  int vratio;
  int vmap_proj;
  int vstdlat1;
  int vstdlat2;
  int vstdlon;
  int vxsouth;
  int vxwest;
  int vconefac;
  int vds;
  int vc_ds;
  int vc_cenlat;
  int vc_cenlon;
  int vptop;
  int vbase_slp;
  int vbase_slt;
  int vbase_lr;
  int pstarcrs;
  int ioptvars[MAX_MM5_OPTS];
  int roptvars[MAX_MM5_OPTS];
  char transfer[256];
  char hostname[64];
  struct hostent *myname;
  time_t xtim;
  t_myvar *vvars;
  int i, j, k, ni, nj, nk, xdim;
  int myvar, mystep;
  int rank[4];
  size_t start[4];
  size_t count[4];
  size_t tstep;
  struct passwd *userinfo;

  /* Some little printout to make us known.... */
  /* Easy to be removed, i hope....            */

  fprintf(stdout, "\n\n");
  fprintf(stdout, "\t*****************************************************\n");
  fprintf(stdout, "\t*                                                   *\n");
  fprintf(stdout, "\t*        University of L'Aquila / CETEMPS           *\n");
  fprintf(stdout, "\t*                  L'Aquila, Italy                  *\n");
  fprintf(stdout, "\t*                                                   *\n");
  fprintf(stdout, "\t*****************************************************\n");
  fprintf(stdout, "\n\n\tComments and feedback to:\n");
  fprintf(stdout, "\t\tGraziano.Giuliani@aquila.infn.it\n\n");

  /* Not so much argument check, sorry */

  if (argc < 2)
  {
    fprintf(stderr, "Not enough args.\n");
    fprintf(stderr, "\tUsage: %s mm5_filename [[tstart] [tstop]]\n", argv[0]);
    return(-1);
  }

  if (MM5_init(argv[1], &file) < 0)
  {
    fprintf(stderr, "Sorry, error reading %s file...\n", argv[1]);
    return(-1);
  }

  if (file.mm5version == 2)
  {
    if (file.mm5_prog != 6)
    {
      fprintf(stderr, "Needs sigma level output.\n");
      return(-1);
    }
  }
  else if (file.mm5version == 3)
  {
    if (file.mm5_prog != 11)
    {
      fprintf(stderr, "Needs sigma level output.\n");
      return(-1);
    }
  }
  else
  {
    fprintf(stderr, "Unknown MM5 I/O format.\n");
    return(-1);
  }

  MM5_get_options(&file, &opt);

  /* Ready to start conversion. */

  if (argc > 2)
    tstart = atoi(argv[2]);
  else
    tstart = STARTSTEP;
  if (argc > 3)
    tstop  = atoi(argv[3]);
  else
    tstop = tstart + NUMSTEPS;

  if (tstart < 0 || tstop < 0 || tstop < tstart)
  {
    fprintf(stderr, "Wrong timestep args...\n");
    MM5_close(&file);
    return(-1);
  }

  fprintf(stdout, "Effectuating conversion....\n");

  charp = basename(argv[1]);
  charp1 = strtok(charp, ".");
  charp2 = strtok(NULL, ".");
  sprintf(outfilename, "%s_%s.%s", charp1, charp2, "nc");

  /* Create output NetCDF file. */

  if (nc_create(outfilename, NC_CLOBBER, &ncid) != NC_NOERR)
  {
    fprintf(stderr, "nc_create: Conversion to NetCDF failed\n");
    return(-1);
  }

  /* define dimensions */

  CHECK_ERROR(nc_def_dim(ncid, "time", NC_UNLIMITED, &dimtime));
  CHECK_ERROR(nc_def_dim(ncid, "i_dot", file.dim_i, &dimidot));
  CHECK_ERROR(nc_def_dim(ncid, "j_dot", file.dim_j, &dimjdot));
  CHECK_ERROR(nc_def_dim(ncid, "i_cross", file.dim_i - 1, &dimicross));
  CHECK_ERROR(nc_def_dim(ncid, "j_cross", file.dim_j - 1, &dimjcross));
  CHECK_ERROR(nc_def_dim(ncid, "sigma_half", file.n_lev, &dimsigmahalf));
  CHECK_ERROR(nc_def_dim(ncid, "sigma_full", file.n_lev + 1, &dimsigmafull));

  /* define variables */

  rank[0] = dimtime;
  actime = inctime(file.reftime, 0);
  sprintf(timeunit, "hours since %d-%d-%d %d",
          actime->tm_year + 1900, actime->tm_mon + 1,
          actime->tm_mday, actime->tm_hour);
  if ((vtime = my_def_var(ncid, NC_FLOAT, 1, rank,
               "time", NULL, timeunit)) < 0)
    return -1;

  rank[0] = dimsigmahalf;
  if ((vsigmahalf = my_def_var(ncid, NC_FLOAT, 1, rank,
           "sigma_level", "Hybrid sigma vertical levels", "dimensionless")) < 0)
    return -1;
  CHECK_ERROR(nc_put_att_text(ncid, vsigmahalf, "positive",
                              strlen("up")+1, "up"));

  rank[0] = dimsigmafull;
  if ((vsigmafull = my_def_var(ncid, NC_FLOAT, 1, rank, "sigma_level_full",
               "Hybrid sigma vertical levels (full)", "dimensionless")) < 0)
    return -1;
  CHECK_ERROR(nc_put_att_text(ncid, vsigmafull, "positive",
                              strlen("up")+1, "up"));

  if ((vc_dim_i = my_def_var(ncid, NC_INT, 0, NULL, "coarse_dim_i",
                  "Coarse domain I dimension", "dimensionless")) < 0)
    return -1;
  if ((vc_dim_j = my_def_var(ncid, NC_INT, 0, NULL, "coarse_dim_j",
                  "Coarse domain J dimension", "dimensionless")) < 0)
    return -1;
  if ((vdim_i = my_def_var(ncid, NC_INT, 0, NULL, "dim_i",
                "Domain I dimension", "dimensionless")) < 0)
    return -1;
  if ((vdim_j = my_def_var(ncid, NC_INT, 0, NULL, "dim_j",
                "Domain J dimension", "dimensionless")) < 0)
    return -1;
  if ((vexp_flag = my_def_var(ncid, NC_INT, 0, NULL, "exp_flag",
                   "Domain expansion flag", "dimensionless")) < 0)
    return -1;
  if ((vexp_dim_i = my_def_var(ncid, NC_INT, 0, NULL, "exp_dim_i",
                    "Expanded dimension I", "dimensionless")) < 0)
    return -1;
  if ((vexp_dim_j = my_def_var(ncid, NC_INT, 0, NULL, "exp_dim_j",
                    "Expanded dimension J", "dimensionless")) < 0)
    return -1;
  if ((vratio = my_def_var(ncid, NC_INT, 0, NULL, "coarse_ratio",
                "Coarse ratio", "dimensionless")) < 0)
    return -1;
  if ((vmap_proj = my_def_var(ncid, NC_INT, 0, NULL, "map_proj_code",
                  "Map projection code", "dimensionless")) < 0)
    return -1;
  if ((vstdlat1 = my_def_var(ncid, NC_FLOAT, 0, NULL, "stdlat_1",
                  "Standard latitude 1", "degree_north")) < 0)
    return -1;
  if ((vstdlat2 = my_def_var(ncid, NC_FLOAT, 0, NULL, "stdlat_2",
                  "Standard latitude 2", "degree_north")) < 0)
    return -1;
  if ((vstdlon = my_def_var(ncid, NC_FLOAT, 0, NULL, "stdlon",
                 "Standard longitude", "degree_east")) < 0)
    return -1;
  if ((vxsouth = my_def_var(ncid, NC_FLOAT, 0, NULL, "xsouth",
                 "I location in coarse of domain (1,1)", "dimensionless")) < 0)
    return -1;
  if ((vxwest = my_def_var(ncid, NC_FLOAT, 0, NULL, "xwest",
                "J location in coarse of domain (1,1)", "dimensionless")) < 0)
    return -1;
  if ((vconefac = my_def_var(ncid, NC_FLOAT, 0, NULL, "conefac",
                  "Cone factor", "dimensionless")) < 0)
    return -1;
  if ((vds = my_def_var(ncid, NC_FLOAT, 0, NULL, "grid_ds",
             "Grid distance", "Km")) < 0)
    return -1;
  if ((vc_ds = my_def_var(ncid, NC_FLOAT, 0, NULL, "coarse_grid_ds",
               "Coarse grid distance", "Km")) < 0)
    return -1;
  if ((vc_cenlat = my_def_var(ncid, NC_FLOAT, 0, NULL, "coarse_cenlat",
                   "Coarse center latitude", "degree_north")) < 0)
    return -1;
  if ((vc_cenlon = my_def_var(ncid, NC_FLOAT, 0, NULL, "coarse_cenlon",
                   "Coarse center longitude", "degree_east")) < 0)
    return -1;
  if ((vptop = my_def_var(ncid, NC_FLOAT, 0, NULL, "ptop",
               "Top pressure in model", "Pa")) < 0)
    return -1;
  if ((vbase_slp = my_def_var(ncid, NC_FLOAT, 0, NULL, "base_slp",
                   "Base state sea level pressure", "Pa")) < 0)
    return -1;
  if ((vbase_slt = my_def_var(ncid, NC_FLOAT, 0, NULL, "base_slt",
                   "Base state sea level temperature", "K")) < 0)
    return -1;
  if ((vbase_lr = my_def_var(ncid, NC_FLOAT, 0, NULL, "base_lr",
                  "Base state temperature lapse rate", "K/ln(Pa)")) < 0)
    return -1;

  for (i = 0; i < opt.icount; i ++)
  {
    if ((ioptvars[i] = my_def_var(ncid, NC_INT, 0, NULL,
                       opt.iopt[i].name, opt.iopt[i].desc, NULL)) < 0)
      return -1;
  }

  for (i = 0; i < opt.rcount; i ++)
  {
    if ((roptvars[i] = my_def_var(ncid, NC_FLOAT, 0, NULL,
                       opt.ropt[i].name, opt.ropt[i].desc, NULL)) < 0)
      return -1;
  }

  vvars = (t_myvar *) malloc(file.total_num * sizeof(t_myvar));
  if (vvars == NULL)
  {
    perror("malloc: ");
    return -1;
  }

  pstarcrs = -1;
  for (myvar = 0; myvar < file.total_num; myvar ++)
  {
    this = &(file.vars[myvar]);
    xdim = this->fdims;
    if (!strcmp(this->fname, "pstarcrs")) pstarcrs = myvar;
    if (!strcmp(this->fname, "terrain")  ||
        !strcmp(this->fname, "land_use") ||
        !strcmp(this->fname, "mapfaccr") ||
        !strcmp(this->fname, "mapfacdt") ||
        !strcmp(this->fname, "coriolis") ||
        !strcmp(this->fname, "pstarcrs") ||
        !strcmp(this->fname, "res_temp") ||
        !strcmp(this->fname, "snowcovr") ||
        !strcmp(this->fname, "soil_t_6") ||
        !strcmp(this->fname, "latitcrs") ||
        !strcmp(this->fname, "longicrs") ||
        !strcmp(this->fname, "latitdot") ||
        !strcmp(this->fname, "longidot"))
    {
      rank[0] = this->fcross ? dimicross : dimidot;
      rank[1] = this->fcross ? dimjcross : dimjdot;
      xdim --;
      vvars[myvar].notimedep = 1;
    }
    else
    {
      rank[0] = dimtime;
      vvars[myvar].notimedep = 0;
      if (xdim == 2)
      {
        rank[1] = this->fcross ? dimicross : dimidot;
        rank[2] = this->fcross ? dimjcross : dimjdot;
      }
      else
      {
        rank[1] = this->full ? dimsigmafull : dimsigmahalf;
        rank[2] = this->fcross ? dimicross : dimidot;
        rank[3] = this->fcross ? dimjcross : dimjdot;
        if (this->fcoupl)
          sprintf(this->funit, "%s", this->funit+4);
      }
    }
    if (!strcmp(this->funit, "k")) sprintf(this->funit, "K");
    if (!strcmp(this->funit, "kpa")) sprintf(this->funit, "kPa");
    if (!strcmp(this->funit, "pa")) sprintf(this->funit, "Pa");
    if (!strcmp(this->funit, "w/m^2")) sprintf(this->funit, "W/m^2");
    if (!strcmp(this->funit, "k/day")) sprintf(this->funit, "K/day");
    if ((vvars[myvar].nc = my_def_var(ncid, NC_FLOAT, xdim + 1, rank,
                           this->fname, this->fdesc, this->funit)) < 0)
      return -1;
    vvars[myvar].mm5 = myvar;
  }

  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "Conventions",
                                strlen("COARDS")+1, "COARDS"));
  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "host",
              strlen("ftp.unidata.ucar.edu")+1, "ftp.unidata.ucar.edu"));
  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "directory",
   strlen("pub/netcdf/Conventions/COARDS")+1, "pub/netcdf/Conventions/COARDS"));

  sprintf(transfer, "MM5 version %d format output on sigma levels",
           file.mm5version);
  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "title",
                              strlen(transfer), transfer));

  gethostname(hostname, 64);
  myname = gethostbyname(hostname);
  time(&xtim);
  userinfo = getpwuid(getuid());
  sprintf(transfer, "Created on %s by %s on %s with command: %s %s",
          myname->h_name, userinfo->pw_name, ctime(&xtim), argv[0], argv[1]);
  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "history",
                              strlen(transfer), transfer));

  sprintf(transfer, "%s", INSTITUTION);
  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "institution",
                              strlen(transfer), transfer));

  sprintf(transfer, "%s %d", "NCAR MM5 model version", file.mm5version);
  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "source",
                              strlen(transfer), transfer));

  sprintf(transfer, "%s", "http://www.mmm.ucar.edu/mm5/mm5-home.html");
  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "references",
                              strlen(transfer), transfer));

  sprintf(transfer, "%s", "No comments yet");
  CHECK_ERROR(nc_put_att_text(ncid, NC_GLOBAL, "comment",
                              strlen(transfer), transfer));

  if (nc_enddef(ncid) != NC_NOERR)
  {
    (void) fprintf(stderr, "Error in define ncdf variables\n");
    return(1);
  }

  CHECK_ERROR(nc_put_var_int(ncid, vc_dim_i, &(file.c_dim_i)));
  CHECK_ERROR(nc_put_var_int(ncid, vc_dim_j, &(file.c_dim_j)));
  CHECK_ERROR(nc_put_var_int(ncid, vdim_i, &(file.dim_i)));
  CHECK_ERROR(nc_put_var_int(ncid, vdim_j, &(file.dim_j)));
  CHECK_ERROR(nc_put_var_int(ncid, vexp_flag, &(file.exp_flag)));
  CHECK_ERROR(nc_put_var_int(ncid, vexp_dim_i, &(file.exp_dim_i)));
  CHECK_ERROR(nc_put_var_int(ncid, vexp_dim_j, &(file.exp_dim_j)));
  CHECK_ERROR(nc_put_var_int(ncid, vratio, &(file.ratio)));
  CHECK_ERROR(nc_put_var_int(ncid, vmap_proj, &(file.map_proj)));
  CHECK_ERROR(nc_put_var_float(ncid, vstdlat1, &(file.stdlat1)));
  CHECK_ERROR(nc_put_var_float(ncid, vstdlat2, &(file.stdlat2)));
  CHECK_ERROR(nc_put_var_float(ncid, vstdlon, &(file.stdlon)));
  CHECK_ERROR(nc_put_var_float(ncid, vxsouth, &(file.xsouth)));
  CHECK_ERROR(nc_put_var_float(ncid, vxwest, &(file.xwest)));
  CHECK_ERROR(nc_put_var_float(ncid, vconefac, &(file.conefac)));
  CHECK_ERROR(nc_put_var_float(ncid, vds, &(file.grid_distance)));
  CHECK_ERROR(nc_put_var_float(ncid, vc_ds, &(file.coarse_grid_d)));
  CHECK_ERROR(nc_put_var_float(ncid, vc_cenlat, &(file.coarse_cenlat)));
  CHECK_ERROR(nc_put_var_float(ncid, vc_cenlon, &(file.coarse_cenlon)));
  CHECK_ERROR(nc_put_var_float(ncid, vptop, &(file.ptop)));
  CHECK_ERROR(nc_put_var_float(ncid, vbase_slp, &(file.basestateslp)));
  CHECK_ERROR(nc_put_var_float(ncid, vbase_slt, &(file.basestateslt)));
  CHECK_ERROR(nc_put_var_float(ncid, vbase_lr, &(file.basestatelapserate)));

  for (i = 0; i < opt.icount; i ++)
    CHECK_ERROR(nc_put_var_int(ncid, ioptvars[i], &(opt.iopt[i].ival)));
  for (i = 0; i < opt.rcount; i ++)
    CHECK_ERROR(nc_put_var_float(ncid, roptvars[i], &(opt.ropt[i].rval)));

  pstar = MM5_getfield(&file, pstarcrs, 0, 0);
  pstard = MM5_c2d(pstar, file.dim_i, file.dim_j);

  start[0] = start[1] = start[2] = start[3] = 0;
  count[1] = count[2] = count[3] = 0;
  count[0] = file.n_lev;
  vlevs = file.vlevs;
  CHECK_ERROR(nc_put_vara_float(ncid, vsigmahalf, start, count, vlevs));

  count[0] = file.n_lev + 1;
  vlevs = malloc((file.n_lev + 1) * sizeof(float));
  vlevs[0] = 0;
  for (i = 0; i < file.n_lev - 1; i ++)
    vlevs[i+1] = (file.vlevs[i] + file.vlevs[i+1]) / 2;
  vlevs[file.n_lev] = 1.0;
  CHECK_ERROR(nc_put_vara_float(ncid, vsigmafull, start, count, vlevs));
  free(vlevs);

  for (mystep = tstart; mystep < tstart + (tstop - tstart); mystep ++)
  {
    actime = inctime(file.reftime, mystep * file.timestep);
    fprintf(stdout, "Writing fields for %s", asctime(actime));

    tstep = mystep - tstart;
    xtime = (float) mystep;
    CHECK_ERROR(nc_put_var1_float(ncid, vtime, &tstep, &xtime));
    nc_sync(ncid);
    for (myvar = 0; myvar < file.total_num; myvar ++)
    {
      this = &(file.vars[myvar]);
      if (vvars[myvar].notimedep == 1)
      {
        value = MM5_getfield(&file, vvars[myvar].mm5, mystep, 0);
        start[1] = start[2] = start[3] = 0;
        count[0] = 1;
        count[1] = this->fcross ? file.dim_j - 1 : file.dim_j;
        count[2] = count[3] = 0;
        ni = this->fcross ? file.dim_i - 1 : file.dim_i;
        for (i = 0; i < ni; i ++)
        {
          start[0] = (size_t) i;
          CHECK_ERROR(nc_put_vara_float(ncid, vvars[myvar].nc,
                      start, count, (value+i*file.dim_j)));
        }
        vvars[myvar].notimedep = -1;
      }
      else if (vvars[myvar].notimedep == 0)
      {
        value = MM5_getfield(&file, vvars[myvar].mm5, mystep, 0);
        start[0] = mystep - tstart;
        count[0] = 1;
        if (this->fdims == 2)
        {
          count[1] = 1;
          count[2] = this->fcross ? file.dim_j - 1 : file.dim_j;
          count[3] = 0; start[2] = 0; start[3] = 0;
          ni = this->fcross ? file.dim_i - 1 : file.dim_i;
          for (i = 0; i < ni; i ++)
          {
            start[1] = (size_t) i;
            CHECK_ERROR(nc_put_vara_float(ncid, vvars[myvar].nc,
                        start, count, (value+i*file.dim_j)));
          }
        }
        else if (this->fdims == 3)
        {
          start[3] = 0;
          count[1] = 1;
          count[2] = 1;
          count[3] = this->fcross ? file.dim_j - 1 : file.dim_j;
          nk = this->full ? file.n_lev + 1 : file.n_lev;
          ni = this->fcross ? file.dim_i - 1 : file.dim_i;
          nj = this->fcross ? file.dim_j - 1 : file.dim_j;
          for (k = 0; k < nk; k ++)
          {
            start[1] = (size_t) k;
            for (i = 0; i < ni; i ++)
            {
              start[2] = (size_t) i;
              if (this->fcoupl)
              {
                for (j = 0; j < nj; j++)
                {
                  if (this->fcross)
                    *(value+k*(file.dim_i*file.dim_j)+i*file.dim_j+j) /=
                    *(pstar+i*file.dim_j+j);
                  else
                    *(value+k*(file.dim_i*file.dim_j)+i*file.dim_j+j) /=
                    *(pstard+i*file.dim_j+j);
                }
              }
              CHECK_ERROR(nc_put_vara_float(ncid, vvars[myvar].nc, start,
                  count, (value+k*(file.dim_i*file.dim_j)+i*file.dim_j)));
            }
          }
        }
      }
    }
  }

  nc_close(ncid);
  free(vvars);

  fprintf(stdout, "Conversion done.\n");
  MM5_close(&file);
  return(0);
}

/* -------------------------------------------------------------------------- */
/*  LOCAL FUNCTION BODY                                                       */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* FUNCTION    : my_def_var                                                   */
/* DESCRIPTION : wrapper around netcdf library for variables definitions      */
/* IN DATA     : netcdf file id, variable type, number of dimensions, rank,   */
/*               var name, description and units                              */
/* OUT DATA    : none                                                         */
/* RETURN      : netcdf id of variables, -1 otherwise                         */
/* -------------------------------------------------------------------------- */
int my_def_var(int ncid, nc_type type, int ndim, int *rank,
               char *name, char *long_name, char *units)
{
  int vid;
  CHECK_ERROR(nc_def_var(ncid, name, type, ndim, rank, &vid));
  if (long_name != NULL)
    CHECK_ERROR(nc_put_att_text(ncid, vid, "long_name",
                                strlen(long_name)+1, long_name));
  if (units != NULL)
    CHECK_ERROR(nc_put_att_text(ncid, vid, "units",
                                strlen(units)+1, units));
  return vid;
}

/* -------------------------------------------------------------------------- */
/* FUNCTION    : inctime                                                      */
/* DESCRIPTION : Returns reftime incremented by inc seconds                   */
/* IN DATA     : reference time                                               */
/* OUT DATA    : none                                                         */
/* RETURN      : reftime + inc seconds                                        */
/* -------------------------------------------------------------------------- */
struct tm *inctime(time_t reftime, int inc)
{
  time_t local;

  setenv("TZ", "UTC", 1);
  tzset();
  local = reftime + inc;
  return gmtime(&local);
}
