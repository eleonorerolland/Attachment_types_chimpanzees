

import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import SpectralClustering
import umap
import matplotlib.pyplot as plt

# Define the file path and title
file = 'YOUR PATH /offspring-reaction-intercept-1-6.csv' #copy paste the path you have the folder and change the \ into /
title = 'Intercept of behavioural reaction of offspring\n UMAP embedding'


# Read data into a dataframe
df = pd.read_csv(file, sep=';', decimal=',')

# Preprocess the data
# Assuming you've already removed empty dimensions and scaled features

# Get the names
names_list = df['Focal']

# Drop the first two columns and get the numerical values into a matrix
df_values = df.drop(['Focal'], axis=1).values


# Perform Spectral Clustering
n_clusters = 3  # Number of clusters to create
clustering = SpectralClustering(n_clusters=n_clusters, assign_labels="discretize", random_state=0)
cluster_labels = clustering.fit_predict(df_values)

# Perform 2D UMAP
NN = 2  # Number of nearest neighbors for UMAP
reducer = umap.UMAP(random_state=21, n_components=2, n_neighbors=NN, min_dist=1)
embedding_2d = reducer.fit_transform(df_values)

# Define custom colors for clusters
custom_colors = ['brown', 'dodgerblue', 'orange']  # You can add more colors as needed

# Create a 2D scatter plot
plt.figure(figsize=(10, 8))

# Plot data points for each cluster with custom colors and labels
for i in range(n_clusters):
    cluster_mask = (cluster_labels == i)
    plt.scatter(embedding_2d[cluster_mask, 0], embedding_2d[cluster_mask, 1],
                c=custom_colors[i], label=f'Attachment type {i + 1}', s=400, marker='o')

# Annotate data points with names --> to get the names with the attributed attachment type, remove the # of the two rows and run again the script
#for i, name in enumerate(names_list):
    #plt.text(embedding_2d[i, 0], embedding_2d[i, 1], name, fontsize=8)



# Set title with increased fontsize
plt.title(f'{title}, NN={NN}', fontsize=25)

# Set xlabel and ylabel with increased fontsize
plt.xlabel('Latent Axis 1', fontsize=25)
plt.ylabel('Latent Axis 2', fontsize=25)

# Set tick parameters for axis numbers
plt.tick_params(axis='both', which='major', labelsize=25)


# Add legend
legend = plt.legend(fontsize=20)  # Change the fontsize as needed

plt.tight_layout()
plt.show()


#save plot
# Create 2D scatter plot
plt.figure(figsize=(10, 8))

# Plot each cluster separately with its color
for i in range(n_clusters):
    cluster_mask = (cluster_labels == i)
    plt.scatter(embedding_2d[cluster_mask, 0], embedding_2d[cluster_mask, 1],
                c=custom_colors[i], label=f'Attachment type {i + 1}', s=400, marker='o')

#(Optional) Annotate points with names
# for i, name in enumerate(names_list):
#     plt.text(embedding_2d[i, 0], embedding_2d[i, 1], name, fontsize=8, ha='right')

# Set plot labels and title
plt.title(f'{title}, NN={NN}', fontsize=25)
plt.xlabel('Latent Axis 1', fontsize=25)
plt.ylabel('Latent Axis 2', fontsize=25)

# Adjust tick size
plt.tick_params(axis='both', which='major', labelsize=25)

# Add legend
plt.legend(fontsize=20, loc="upper right", frameon=True)

# Optimize layout before saving
plt.tight_layout()

#  Save the plot as an editable **SVG vector file**
plt.savefig(r"C:/Users/erolland/Desktop/WORK AND PHD 2/MY PHD/Paper 1/Nature Human Behaviour/Documents and files for code/Data files/Figure3a.svg", format="svg", dpi=300)



#  Show the plot (AFTER saving, to avoid blank SVG)
plt.show()













