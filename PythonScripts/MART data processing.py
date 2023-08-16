from zipfile import ZipFile
import pandas as pd
import os
from datetime import datetime

# zf = ZipFile('/Users/seo-b/Downloads/111.zip')
#
# source_chunks = pd.read_csv(
#   zf.open('mart_djy_03.txt'),
#   sep='|',
#   encoding='CP949',
#   header=None,
#   dtype='string',
#   chunksize=500_000
# )

#
#
source_chunks = pd.read_csv(
  '/Users/seo-b/Downloads/mart_djy_03.txt',
  sep='|',
  encoding='CP949',
  header=None,
  dtype='string',
  chunksize=500_000
)

full_data = pd.concat(source_chunks, ignore_index=True)

full_data.to_csv("MART_DJY.csv", index=False)