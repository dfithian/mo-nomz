//
//  AddPhotoControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/31/22.
//

import UIKit

class AddPhotoController: UIViewController {
    @IBOutlet weak var switcher: UIBarButtonItem!

    var navigationVc: AddController? = nil
    var pickVc: PickPhotoController? = nil
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        performSegue(withIdentifier: "pushManualRecipe", sender: nil)
    }
    
    private func setupSwitcher() {
        switcher.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Add link", image: UIImage(systemName: "link"), state: .off, handler: { _ in self.navigationVc?.switchToLink() }),
            UIAction(title: "Add manual", image: UIImage(systemName: "pencil"), state: .off, handler: { _ in self.navigationVc?.switchToManual() }),
            UIAction(title: "Add photo", image: UIImage(systemName: "photo.on.rectangle"), state: .on, handler: { _ in self.navigationVc?.switchToPhoto() })
        ])
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? PickPhotoController, segue.identifier == "embedPhotos" {
            pickVc = vc
        }
        if let vc = segue.destination as? AddManualController, segue.identifier == "pushManualRecipe" {
            vc.change = .photoReview
            vc.name = "FIXME NAME"
            vc.link = "FIXME LINK"
            vc.isRecipe = true
            vc.isActive = true
            vc.ingredients = "FIXME INGREDIENTS"
            vc.steps = "FIXME STEPS"
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupSwitcher()
    }
}

class PickPhotoController: UICollectionViewController, UICollectionViewDelegateFlowLayout, UIImagePickerControllerDelegate, UINavigationControllerDelegate {
    var images: [UIImage] = []
    
    @objc func didTapAddPhoto(_ sender: Any?) {
        let picker = UIImagePickerController()
        picker.delegate = self
        present(picker, animated: true)
    }
    
    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return images.count + 1
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        if indexPath.row < images.count {
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "imageItem", for: indexPath) as! OneImage
            cell.image.image = images[indexPath.row]
            return cell
        } else {
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "addImage", for: indexPath) as! OneImageButton
            cell.layer.cornerRadius = 10
            cell.button.addTarget(self, action: #selector(didTapAddPhoto), for: .touchUpInside)
            return cell
        }
    }
    
    override func collectionView(_ collectionView: UICollectionView, contextMenuConfigurationForItemAt: IndexPath, point: CGPoint) -> UIContextMenuConfiguration? {
        guard contextMenuConfigurationForItemAt.row < images.count else { return nil }
        let actions: UIContextMenuActionProvider = { _ in
            return UIMenu(children: [
                UIAction(title: "Remove", attributes: .destructive, handler: { _ in
                    self.images.remove(at: contextMenuConfigurationForItemAt.row)
                })
            ])
        }
        return UIContextMenuConfiguration(identifier: nil, previewProvider: nil, actionProvider: actions)
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        guard indexPath.row >= images.count else { return }
        didTapAddPhoto(nil)
    }
    
    func collectionView(_ collectionView: UICollectionView, layout: UICollectionViewLayout, sizeForItemAt: IndexPath) -> CGSize {
        let size = (view.frame.size.width / 3) - 10
        return CGSize(width: size, height: size)
    }
    
    // FIXME this should push a controller on the navigation stack to review the screen grab?
    func imagePickerController(_ picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [UIImagePickerController.InfoKey : Any]) {
        if let image = info[.originalImage] as? UIImage {
            images.append(image)
        }
        dismiss(animated: true, completion: nil)
        collectionView.reloadData()
    }
}
