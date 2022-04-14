//
//  RecipeFilterController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 3/6/22.
//

import UIKit

protocol RecipeFilterDelegate {
    func updateSelectedTag(tag: String?)
}

class RecipeFilterController: UICollectionViewController, UICollectionViewDelegateFlowLayout {
    var delegate: RecipeFilterDelegate? = nil
    var tags: [String]? = nil
    var selected: String? = nil
    var onChange: (() -> Void)? = nil

    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return tags?.count ?? 0
    }

    private func sendUpdates() {
        delegate?.updateSelectedTag(tag: selected)
        DispatchQueue.main.async {
            self.collectionView.reloadData()
        }
    }

    func clear() {
        selected = nil
        sendUpdates()
    }

    private func toggleTag(_ index: Int) {
        let tag_ = tags![index]
        if selected == tag_ {
            selected = nil
        } else {
            selected = tag_
        }
        sendUpdates()
    }

    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "tagButton", for: indexPath) as! OneCellButton
        let tag_ = tags![indexPath.row]
        cell.button.tag = indexPath.row
        cell.button.setTitle(tag_, for: .normal)
        cell.button.layer.cornerRadius = 5
        if selected == tag_ {
            cell.button.backgroundColor = UIColor.systemGray5
        } else {
            cell.button.backgroundColor = nil
        }
        return cell
    }

    override func collectionView(_ collectionView: UICollectionView, contextMenuConfigurationForItemAt indexPath: IndexPath, point: CGPoint) -> UIContextMenuConfiguration? {
        guard let tag_ = tags?[indexPath.row] else { return nil }
        return UIContextMenuConfiguration(identifier: nil, previewProvider: nil, actionProvider: { _ in
                UIMenu(children: [
                    UIAction(title: "Edit tag", image: UIImage(systemName: "pencil"), handler: { _ in
                        self.promptGetInput(title: "Edit tag", content: tag_, configure: nil, completion: { (new) in
                            Database.updateTag(old: tag_, new: new)
                            self.onChange?()
                        })
                    }),
                    UIAction(title: "Delete tag", image: UIImage(systemName: "xmark"), attributes: .destructive, handler: { _ in
                        self.promptForConfirmation(title: "Delete tag", message: "This tag will be removed from all recipes. Do you want to continue?", handler: { _ in
                            Database.deleteTag(old: tag_)
                            self.onChange?()
                        })
                    })
                ])
        })
    }

    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        toggleTag(indexPath.row)
    }

    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let text = tags?[indexPath.row] ?? ""
        let font = UIFont.systemFont(ofSize: 12)
        let fontAttributes = [NSAttributedString.Key.font: font]
        let buttonWidth = 25.0
        let width = (text as NSString).size(withAttributes: fontAttributes as [NSAttributedString.Key : Any]).width + buttonWidth
        let height = view.frame.size.height - 10
        return CGSize(width: width, height: height)
    }

    private func loadData() {
        tags = Database.getAllTags()
    }

    func reloadData() {
        loadData()
        DispatchQueue.main.async {
            self.collectionView.reloadData()
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        loadData()
    }
}
