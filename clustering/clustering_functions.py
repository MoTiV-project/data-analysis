

from scipy import stats
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
from yellowbrick.cluster import KElbowVisualizer
from sklearn.cluster import AgglomerativeClustering


# Standardize features by removing the mean and scaling to unit variance
def standardize_data(df_tr):
    ''' standardize data for the clustering. 
        The input dataset should have only the data useful for clustering (no index or id)
    '''
    
    data = stats.zscore(df_tr)
    data[np.isnan(data)] = 0

    # Standardize features by removing the mean and scaling to unit variance
    X_transformed = StandardScaler().fit_transform(data)
    return(X_transformed)

# KMeans - Elbow methon 
def kmeans_elbow(df, metric = 'distortion', save_fig = False):
    ''' Input: scaled data
        Run the elbow method and returns the optimal number of clusters 
        Other metrics available: 'silhouette' and 'calinski_harabasz'
        Metric:
        - **distortion**: mean sum of squared distances to centers
        - **silhouette**: mean ratio of intra-cluster and nearest-cluster distance
        - **calinski_harabasz**: ratio of within to between cluster dispersion
    '''
    model = KMeans()
    visualizer = KElbowVisualizer(model, k=(2,12), metric=metric)
    visualizer.fit(df)        # Fit the data to the visualizer
    visualizer.show()
    k=visualizer.elbow_value_ # optimal number of clusters
    return(k)

# KMeans 
def kmeans(k, df):
    
    km = KMeans(n_clusters=k)
    labels = km.fit_predict(df)
    centroids = km.cluster_centers_
    return(labels, centroids)

    
# Hierarchical 
def hierarchical(df, k, affinity='euclidean',  linkage='single'):
    hier = AgglomerativeClustering(n_clusters=k, affinity=affinity, linkage=linkage)
    labels = hier.fit_predict(df)
    return(labels)


# hdbscan
#def hdbscan_cluster(df):
#    clusterer = hdbscan.HDBSCAN()
#    clusterer.fit(df)
    
#    return()