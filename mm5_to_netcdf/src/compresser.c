/* ************************************************************************** */
/*                                                                            */
/*                                compresser.c                                */
/*                                                                            */
/* ************************************************************************** */
 
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* RCS id string                                                              */
   const static char rcs_id_string[] =
    "$Id: compresser.c,v 1.2 2001/10/30 09:35:46 graziano Exp $";
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
/* FILENAME   : compresser.c                                                  */
/* AUTHOR     : Graziano Giuliani                                             */
/* DESCRIPTION: Compress MM5 NetCDF archived data                             */
/* DOCUMENTS  : See README file                                               */
/*                                                                            */
/* ************************************************************************** */
 
/* -------------------------------------------------------------------------- */
/*  HEADER FILES                                                              */
/* -------------------------------------------------------------------------- */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <netdb.h>
#include <string.h>
#include <libgen.h>
#include <netcdf.h>

/* -------------------------------------------------------------------------- */
/*  DEFINITIONS                                                               */
/* -------------------------------------------------------------------------- */
#define CHECK_ERROR(nc_stat) \
  if (nc_stat != NC_NOERR) \
  { \
    fprintf(stderr, "Error netcdf = %s\n", nc_strerror(nc_stat)); \
    return(-1); \
  }

#define DEBUG 1
#define COMP_SIZE    short
#define COMP_DEF     NC_SHORT

/* -------------------------------------------------------------------------- */
/* LOCAL FUNCTION PROTOTYPES                                                  */
/* -------------------------------------------------------------------------- */
static int my_get_att(int ncid, nc_type type, int varid, char *name,
                      int len, void **dest);
static int my_put_att(int ncid, nc_type type, int varid,
                      char *name, int len, void *source);
static int my_get_var(int ncid, nc_type type, int varid,
                      size_t len, void **dest);
static int my_put_var(int ncid, nc_type type, int varid, void *source);
static void find_max_min(void *vals, nc_type type, size_t len, 
                         float *max, float *min);

/* -------------------------------------------------------------------------- */
/* PROGRAM     : compresser                                                   */
/* DESCRIPTION : Compress MM5 NetCDF format archived files                    */
/* IN DATA     : Command line arguments :                                     */
/*               Name of MM5 NetCDF format file                               */
/* OUT DATA    : none                                                         */
/* RETURN      : 0 on completion, -1 otherwise                                */
/* -------------------------------------------------------------------------- */
int main(int argc, char *argv[])
{
  char outfilename[256];
  char *charp, *charp1;
  int ncid;
  int ncid_in;
  int ndims;
  int nvars;
  int natts;
  char name[NC_MAX_NAME];
  int unlimdimid;
  char *buff;
  float *realval;
  int i, j;
  size_t *dimlen;
  int *dimids;
  int *varids;
  int vardims;
  int varatts;
  int vardimids[4];
  size_t len;
  size_t size;
  nc_type xtype;
  int tmp;
  float maxval, minval;
  float scale_factor;

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
    fprintf(stderr, "\tUsage: %s netcdf_filename\n", argv[0]);
    return(-1);
  }

  if (nc_open(argv[1], NC_NOWRITE, &ncid_in) != NC_NOERR)
  {
    fprintf(stderr, "nc_open: Cannot open input NetCDF file !\n");
    return(-1);
  }

  /* Ready to start compression. */

  fprintf(stdout, "Effectuating compression....\n");

  charp = basename(argv[1]);
  charp1 = strtok(charp, ".");
  sprintf(outfilename, "%s_%s.%s", charp1, "z", "nc");

  /* Create output NetCDF file. */

  if (nc_create(outfilename, NC_CLOBBER, &ncid) != NC_NOERR)
  {
    fprintf(stderr, "nc_create: Conversion to NetCDF failed\n");
    return(-1);
  }

  CHECK_ERROR(nc_inq(ncid_in, &ndims, &nvars, &natts, &unlimdimid));

  if (DEBUG)
  {
    fprintf(stdout, "Input file : %s\n", argv[1]);
    fprintf(stdout, "Number of dimensions: %d\n", ndims);
    fprintf(stdout, "Number of variables: %d\n", nvars);
    fprintf(stdout, "Number of attributes: %d\n", natts);
    fprintf(stdout, "Unlimited dimension ID: %d\n", unlimdimid);
  }

  dimlen = (size_t *) malloc(ndims * sizeof(size_t));
  dimids = (int *) malloc(ndims * sizeof(int));
  if (dimlen == NULL || dimids == NULL)
  {
    fprintf(stderr, "malloc: %s\n", strerror(errno));
    return(-1);
  }

  for (i = 0; i < ndims; i ++)
  {
    CHECK_ERROR(nc_inq_dim(ncid_in, i, name, &dimlen[i]));
    CHECK_ERROR(nc_def_dim(ncid, name, dimlen[i], dimids + i));
    if (DEBUG)
      fprintf(stdout, "Defining dimension %s of len %d\n", name, dimlen[i]);
  }

  for (i = 0; i < natts; i ++)
  {
    CHECK_ERROR(nc_inq_attname(ncid_in, NC_GLOBAL, i, name));
    CHECK_ERROR(nc_inq_att(ncid_in, NC_GLOBAL, name, &xtype, &len));
    if (my_get_att(ncid_in, xtype, NC_GLOBAL, name, len, (void *) &buff))
    {
      fprintf(stderr, "Error retrieving global attribute %s\n", name);
      return -1;
    }
    if (!strcmp(name, "history"))
    {
      char *ctmp;
      char *app;
      char msg[256];
      time_t xtim;

      app = buff;
      time(&xtim);
      sprintf(msg, "\nPrecision reduced to %d bytes on %s using command: %s %s",
              sizeof(COMP_SIZE), ctime(&xtim), argv[0], argv[1]);
      len = len + strlen(msg) + 1;
      ctmp = (char *) malloc(len);
      sprintf(ctmp, "%s%s", buff, msg);
      buff = ctmp;
      free(app);
    }
    if (my_put_att(ncid, xtype, NC_GLOBAL, name, len, buff))
    {
      fprintf(stderr, "Error putting global attribute %s\n", name);
      return -1;
    }
    free(buff);
    if (DEBUG)
      fprintf(stdout, "Adding global attribute %s\n", name);
  }

  varids = (int *) malloc(nvars * sizeof(int));
  if (varids == NULL)
  {
    fprintf(stderr, "malloc: %s\n", strerror(errno));
    return(-1);
  }

  for (i = 0; i < nvars; i ++)
  {
    CHECK_ERROR(nc_inq_var(ncid_in, i, name, &xtype, &vardims,
                           vardimids, &varatts));
    if (vardims > 1) xtype = NC_SHORT;
    CHECK_ERROR(nc_def_var(ncid, name, xtype, vardims, vardimids, varids+i));
    if (DEBUG)
      fprintf(stdout, "Defining variable %s as var %d\n", name, *(varids+i));
    for (j = 0; j < varatts; j ++)
    {
      CHECK_ERROR(nc_inq_attname(ncid_in, i, j, name));
      CHECK_ERROR(nc_inq_att(ncid_in, i, name, &xtype, &len));
      if (my_get_att(ncid_in, xtype, i, name, len, (void *) &buff))
      {
        fprintf(stderr, "Error retrieving attribute %s for var %d\n", name, i);
        return -1;
      }
      if (my_put_att(ncid, xtype, *(varids+i), name, len, buff))
      {
        fprintf(stderr, "Error putting attribute %s for var %d\n",
                name, *(varids+i));
        return -1;
      }
      free(buff);
      if (DEBUG)
        fprintf(stdout, "Adding attribute %s for var %d\n", name, *(varids+i));
    }
  }

  nc_enddef(ncid);

  for (i = 0; i < nvars; i ++)
  {
    size = 1;
    CHECK_ERROR(nc_inq_var(ncid_in, i, name, &xtype, &vardims,
                           vardimids, &varatts));
    for (j = 0; j < vardims; j ++)
      size *= dimlen[vardimids[j]];
    if (DEBUG)
      fprintf(stdout, "Variable %s is total size of %d\n", name, size);
    if (vardims > 1)
    {
      buff = (char *) calloc(size, sizeof(COMP_SIZE));
      if (buff == NULL)
      {
        fprintf(stderr, "malloc: %s\n", strerror(errno));
        return(-1);
      }
      if (DEBUG)
        fprintf(stdout, "Reading variable\n");
      if (my_get_var(ncid_in, xtype, i, size, (void *) &realval))
      {
        fprintf(stderr, "Error retrieving variable %d\n", i);
        return -1;
      }
      if (DEBUG)
        fprintf(stdout, "Example = %f\n", *realval);
      find_max_min((void *) realval, xtype, size, &maxval, &minval);
      if (maxval-minval == 0)
        scale_factor = 1.0;
      else if (maxval-minval < 0)
      {
        scale_factor = maxval;
        maxval = minval;
        minval = scale_factor;
      }
      else
        scale_factor = (maxval-minval)/((float)(256*sizeof(COMP_SIZE)));
      if (!strcmp(name,"land_use")  ||
          !strcmp(name,"snowcovr") ||
          !strcmp(name,"regime")    )
      {
        minval = 0.0;
        scale_factor = 1.000;
      }
      if (DEBUG)
      {
        fprintf(stdout, "Maxval = %f, Minval = %f\n", maxval, minval);
        fprintf(stdout, "Range for variable %s is %f\n", name, maxval-minval);
        fprintf(stdout, "Resolution will be %f\n", scale_factor);
      }

      for (j = 0; j < size; j ++)
      {
        tmp = (int) ((realval[j] - minval) / scale_factor);
        *((COMP_SIZE *) (buff + j * sizeof(COMP_SIZE))) = tmp;
      }

      nc_redef(ncid);
      CHECK_ERROR(nc_put_att_float(ncid, *(varids+i), "add_offset",
                  NC_FLOAT, 1, &minval));
      CHECK_ERROR(nc_put_att_float(ncid, *(varids+i), "scale_factor",
                  NC_FLOAT, 1, &scale_factor));
      nc_enddef(ncid);

      if (my_put_var(ncid, COMP_DEF, *(varids+i), (void *) buff))
      {
        fprintf(stderr, "Error putting variable %d\n", i);
        return -1;
      }
      free(realval);
      free(buff);
    }
    else
    {
      if (my_get_var(ncid_in, xtype, i, size, (void *) &buff))
      {
        fprintf(stderr, "Error retrieving variable %d\n", i);
        return -1;
      }
      if (my_put_var(ncid, xtype, *(varids+i), (void *) buff))
      {
        fprintf(stderr, "Error putting variable %d\n", i);
        return -1;
      }
      free(buff);
    }
  }
  
  nc_close(ncid_in);
  nc_close(ncid);

  free(dimlen);
  free(dimids);
  free(varids);

  fprintf(stdout, "Conversion done.\n");
  return(0);
}

/* -------------------------------------------------------------------------- */
/*  LOCAL FUNCTION BODY                                                       */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* FUNCTION    : my_get_att                                                   */
/* DESCRIPTION : wrapper around netcdf library for attribute retrieval        */
/* IN DATA     : netcdf file id, attribute type, attribute len                */
/* OUT DATA    : destination                                                  */
/* RETURN      : netcdf id of variables, -1 otherwise                         */
/* -------------------------------------------------------------------------- */
int my_get_att(int ncid, nc_type type, int varid, char *name,
               int len, void **dest)
{
  switch (type)
  {
    case NC_BYTE:
     {
       unsigned char *tmp;
       tmp = (unsigned char *) malloc(len);
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_att_uchar(ncid, varid, name, tmp));
       *dest = tmp;
     }
     break;
    case NC_SHORT:
     {
       short *tmp;
       tmp = (short *) malloc(len * sizeof(short));
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_att_short(ncid, varid, name, tmp));
       *dest = tmp;
     }
     break;
    case NC_INT:
     {
       int *tmp;
       tmp = (int *) malloc(len * sizeof(int));
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_att_int(ncid, varid, name, tmp));
       *dest = tmp;
     }
     break;
    case NC_FLOAT:
     {
       float *tmp;
       tmp = (float *) malloc(len * sizeof(float));
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_att_float(ncid, varid, name, tmp));
       *dest = tmp;
     }
     break;
    case NC_DOUBLE:
     {
       double *tmp;
       tmp = (double *) malloc(len * sizeof(double));
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_att_double(ncid, varid, name, tmp));
       *dest = tmp;
     }
     break;
    case NC_CHAR:
     {
       char *tmp;
       tmp = (char *) malloc(len);
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_att_text(ncid, varid, name, tmp));
       *dest = tmp;
     }
     break;
    default:
     fprintf(stderr, "Wrong or not implemented type for attribute.\n");
     return -1;
  }
  return 0;
}

/* -------------------------------------------------------------------------- */
/* FUNCTION    : my_put_att                                                   */
/* DESCRIPTION : wrapper around netcdf library for attribute putting          */
/* IN DATA     : netcdf file id, attribute type, attribute memory             */
/* OUT DATA    : none                                                         */
/* RETURN      : netcdf id of variables, -1 otherwise                         */
/* -------------------------------------------------------------------------- */
int my_put_att(int ncid, nc_type type, int varid, char *name,
               int len, void *source)
{
  if (source == NULL)
  {
    fprintf(stderr, "Wrong memory source for attribute %s\n", name);
    return -1;
  }

  switch (type)
  {
    case NC_BYTE:
     {
       unsigned char *tmp;
       tmp = (unsigned char *) source;
       CHECK_ERROR(nc_put_att_uchar(ncid, varid, name, type, len, tmp));
     }
     break;
    case NC_SHORT:
     {
       short *tmp;
       tmp = (short *) source;
       CHECK_ERROR(nc_put_att_short(ncid, varid, name, type, len, tmp));
     }
     break;
    case NC_INT:
     {
       int *tmp;
       tmp = (int *) source;
       CHECK_ERROR(nc_put_att_int(ncid, varid, name, type, len, tmp));
     }
     break;
    case NC_FLOAT:
     {
       float *tmp;
       tmp = (float *) source;
       CHECK_ERROR(nc_put_att_float(ncid, varid, name, type, len, tmp));
     }
     break;
    case NC_DOUBLE:
     {
       double *tmp;
       tmp = (double *) source;
       CHECK_ERROR(nc_put_att_double(ncid, varid, name, type, len, tmp));
     }
     break;
    case NC_CHAR:
     {
       char *tmp;
       tmp = (char *) source;
       CHECK_ERROR(nc_put_att_text(ncid, varid, name, len, tmp));
     }
     break;
    default:
     fprintf(stderr, "Wrong or not implemented type for attribute.\n");
     return -1;
  }
  return 0;
}

/* -------------------------------------------------------------------------- */
/* FUNCTION    : my_get_var                                                   */
/* DESCRIPTION : wrapper around netcdf library for variable retrieval         */
/* IN DATA     : netcdf file id, variable type, variable len                  */
/* OUT DATA    : destination                                                  */
/* RETURN      : netcdf id of variables, -1 otherwise                         */
/* -------------------------------------------------------------------------- */
int my_get_var(int ncid, nc_type type, int varid, size_t len, void **dest)
{
  switch (type)
  {
    case NC_BYTE:
     {
       unsigned char *tmp;
       tmp = (unsigned char *) malloc(len);
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_var_uchar(ncid, varid, tmp));
       *dest = tmp;
     }
     break;
    case NC_SHORT:
     {
       short *tmp;
       tmp = (short *) malloc(len * sizeof(short));
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_var_short(ncid, varid, tmp));
       *dest = tmp;
     }
     break;
    case NC_INT:
     {
       int *tmp;
       tmp = (int *) malloc(len * sizeof(int));
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_var_int(ncid, varid, tmp));
       *dest = tmp;
     }
     break;
    case NC_FLOAT:
     {
       float *tmp;
       tmp = (float *) malloc(len * sizeof(float));
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_var_float(ncid, varid, tmp));
       *dest = tmp;
     }
     break;
    case NC_DOUBLE:
     {
       double *tmp;
       tmp = (double *) malloc(len * sizeof(double));
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_var_double(ncid, varid, tmp));
       *dest = tmp;
     }
     break;
    case NC_CHAR:
     {
       char *tmp;
       tmp = (char *) malloc(len);
       if (tmp == NULL)
       {
         fprintf(stderr, "malloc: %s\n", strerror(errno));
         return -1;
       }
       CHECK_ERROR(nc_get_var_text(ncid, varid, tmp));
       *dest = tmp;
     }
     break;
    default:
     fprintf(stderr, "Wrong or not implemented type for variable.\n");
     return -1;
  }
  return 0;
}

/* -------------------------------------------------------------------------- */
/* FUNCTION    : my_put_var                                                   */
/* DESCRIPTION : wrapper around netcdf library for variable putting           */
/* IN DATA     : netcdf file id, variable type, variable memory               */
/* OUT DATA    : none                                                         */
/* RETURN      : netcdf id of variables, -1 otherwise                         */
/* -------------------------------------------------------------------------- */
int my_put_var(int ncid, nc_type type, int varid, void *source)
{
  if (source == NULL)
  {
    fprintf(stderr, "Wrong memory source for variable %d\n", varid);
    return -1;
  }

  switch (type)
  {
    case NC_BYTE:
     {
       unsigned char *tmp;
       tmp = (unsigned char *) source;
       CHECK_ERROR(nc_put_var_uchar(ncid, varid, tmp));
     }
     break;
    case NC_SHORT:
     {
       short *tmp;
       tmp = (short *) source;
       CHECK_ERROR(nc_put_var_short(ncid, varid, tmp));
     }
     break;
    case NC_INT:
     {
       int *tmp;
       tmp = (int *) source;
       CHECK_ERROR(nc_put_var_int(ncid, varid, tmp));
     }
     break;
    case NC_FLOAT:
     {
       float *tmp;
       tmp = (float *) source;
       CHECK_ERROR(nc_put_var_float(ncid, varid, tmp));
     }
     break;
    case NC_DOUBLE:
     {
       double *tmp;
       tmp = (double *) source;
       CHECK_ERROR(nc_put_var_double(ncid, varid, tmp));
     }
     break;
    case NC_CHAR:
     {
       char *tmp;
       tmp = (char *) source;
       CHECK_ERROR(nc_put_var_text(ncid, varid, tmp));
     }
     break;
    default:
     fprintf(stderr, "Wrong or not implemented type for variable.\n");
     return -1;
  }
  return 0;
}

/* -------------------------------------------------------------------------- */
/* FUNCTION    : find_max_min                                                 */
/* DESCRIPTION : finds maximum and minimum values in array                    */
/* IN DATA     : array pointer, type of data, size                            */
/* OUT DATA    : max and min values as floats                                 */
/* RETURN      : none                                                         */
/* -------------------------------------------------------------------------- */
void find_max_min(void *vals, nc_type type, size_t len, 
                  float *max, float *min)
{
  int i, nvals;
  float lmax, lmin;

  *max = 0;
  *min = 0;

  if (vals == NULL) return;
  switch (type)
  {
    case NC_BYTE:
     {
       unsigned char *tmp;
       tmp = (unsigned char *) vals;
       nvals = len / sizeof(unsigned char);
       lmax = lmin = *tmp;
       for (i = 0; i < nvals; i ++)
       {
         if ((float) *(tmp+i) > lmax) lmax = (float) *(tmp+i);
         if ((float) *(tmp+i) < lmin) lmin = (float) *(tmp+i);
       }
     }
     break;
    case NC_SHORT:
     {
       short *tmp;
       tmp = (short *) vals;
       nvals = len / sizeof(short);
       lmax = lmin = *tmp;
       for (i = 0; i < nvals; i ++)
       {
         if ((float) *(tmp+i) > lmax) lmax = (float) *(tmp+i);
         if ((float) *(tmp+i) < lmin) lmin = (float) *(tmp+i);
       }
     }
     break;
    case NC_INT:
     {
       int *tmp;
       tmp = (int *) vals;
       nvals = len / sizeof(int);
       lmax = lmin = *tmp;
       for (i = 0; i < nvals; i ++)
       {
         if ((float) *(tmp+i) > lmax) lmax = (float) *(tmp+i);
         if ((float) *(tmp+i) < lmin) lmin = (float) *(tmp+i);
       }
     }
     break;
    case NC_FLOAT:
     {
       float *tmp;
       tmp = (float *) vals;
       nvals = len / sizeof(float);
       lmax = lmin = *tmp;
       for (i = 0; i < nvals; i ++)
       {
         if ((float) *(tmp+i) > lmax) lmax = (float) *(tmp+i);
         if ((float) *(tmp+i) < lmin) lmin = (float) *(tmp+i);
       }
     }
     break;
    case NC_DOUBLE:
     {
       double *tmp;
       tmp = (double *) vals;
       nvals = len / sizeof(double);
       lmax = lmin = *tmp;
       for (i = 0; i < nvals; i ++)
       {
         if ((float) *(tmp+i) > lmax) lmax = (float) *(tmp+i);
         if ((float) *(tmp+i) < lmin) lmin = (float) *(tmp+i);
       }
     }
     break;
    case NC_CHAR:
     {
       char *tmp;
       tmp = (char *) vals;
       nvals = len / sizeof(char);
       lmax = lmin = *tmp;
       for (i = 0; i < nvals; i ++)
       {
         if ((float) *(tmp+i) > lmax) lmax = (float) *(tmp+i);
         if ((float) *(tmp+i) < lmin) lmin = (float) *(tmp+i);
       }
     }
     break;
    default:
     fprintf(stderr, "Wrong or not implemented type for variable.\n");
     return;
  }

  *max = ceil(lmax);
  *min = floor(lmin);
  return;
}
